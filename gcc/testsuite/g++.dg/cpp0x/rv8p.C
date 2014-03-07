// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile { target c++11 } }
// { dg-skip-if "packed attribute missing for struct one/three/five/seven" { "epiphany-*-*" } { "*" } { "" } }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {long x[1];};
struct two   {long x[2];};
struct three {long x[3];};
struct four  {long x[4];};
struct five  {long x[5];};
struct six   {long x[6];};
struct seven {long x[7];};
struct eight {long x[8];};

struct A
{
    A();
    A(const volatile A&&);
};

               A    source();
const          A  c_source();
      volatile A  v_source();
const volatile A cv_source();

// 8 at a time

one   sink_8_12345678(               A&);
two   sink_8_12345678(const          A&);
three sink_8_12345678(volatile       A&);
four  sink_8_12345678(const volatile A&);
five  sink_8_12345678(               A&&);
six   sink_8_12345678(const          A&&);
seven sink_8_12345678(volatile       A&&);
eight sink_8_12345678(const volatile A&&);

int test8_12345678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_8_12345678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_8_12345678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_8_12345678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_8_12345678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_8_12345678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_8_12345678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_8_12345678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_8_12345678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test8_12345678();
}
