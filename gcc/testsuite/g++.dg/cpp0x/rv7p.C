// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile }
// { dg-options "-std=c++11" }
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

// 7 at a time

one   sink_7_1234567(               A&);
two   sink_7_1234567(const          A&);
three sink_7_1234567(volatile       A&);
four  sink_7_1234567(const volatile A&);
five  sink_7_1234567(               A&&);
six   sink_7_1234567(const          A&&);
seven sink_7_1234567(volatile       A&&);

int test7_1234567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1234567(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1234567(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_1234567(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_1234567(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_1234567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_1234567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_1234567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_7_1234568(               A&);
two   sink_7_1234568(const          A&);
three sink_7_1234568(volatile       A&);
four  sink_7_1234568(const volatile A&);
five  sink_7_1234568(               A&&);
six   sink_7_1234568(const          A&&);
eight sink_7_1234568(const volatile A&&);

int test7_1234568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1234568(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1234568(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_1234568(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_1234568(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_1234568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_1234568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_1234568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_7_1234568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_7_1234578(               A&);
two   sink_7_1234578(const          A&);
three sink_7_1234578(volatile       A&);
four  sink_7_1234578(const volatile A&);
five  sink_7_1234578(               A&&);
seven sink_7_1234578(volatile       A&&);
eight sink_7_1234578(const volatile A&&);

int test7_1234578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1234578(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1234578(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_1234578(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_1234578(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_1234578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_1234578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_7_1234578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_7_1234578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_7_1234678(               A&);
two   sink_7_1234678(const          A&);
three sink_7_1234678(volatile       A&);
four  sink_7_1234678(const volatile A&);
six   sink_7_1234678(const          A&&);
seven sink_7_1234678(volatile       A&&);
eight sink_7_1234678(const volatile A&&);

int test7_1234678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1234678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1234678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_1234678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_1234678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_1234678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_1234678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_7_1234678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_7_1235678(               A&);
two   sink_7_1235678(const          A&);
three sink_7_1235678(volatile       A&);
five  sink_7_1235678(               A&&);
six   sink_7_1235678(const          A&&);
seven sink_7_1235678(volatile       A&&);
eight sink_7_1235678(const volatile A&&);

int test7_1235678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1235678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1235678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_1235678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_1235678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_1235678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_1235678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_7_1235678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_7_1245678(               A&);
two   sink_7_1245678(const          A&);
four  sink_7_1245678(const volatile A&);
five  sink_7_1245678(               A&&);
six   sink_7_1245678(const          A&&);
seven sink_7_1245678(volatile       A&&);
eight sink_7_1245678(const volatile A&&);

int test7_1245678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1245678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1245678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_1245678(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_7_1245678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_1245678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_1245678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_1245678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_7_1245678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_7_1345678(               A&);
three sink_7_1345678(volatile       A&);
four  sink_7_1345678(const volatile A&);
five  sink_7_1345678(               A&&);
six   sink_7_1345678(const          A&&);
seven sink_7_1345678(volatile       A&&);
eight sink_7_1345678(const volatile A&&);

int test7_1345678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_1345678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_7_1345678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_7_1345678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_1345678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_1345678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_1345678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_1345678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_7_1345678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_7_2345678(const          A&);
three sink_7_2345678(volatile       A&);
four  sink_7_2345678(const volatile A&);
five  sink_7_2345678(               A&&);
six   sink_7_2345678(const          A&&);
seven sink_7_2345678(volatile       A&&);
eight sink_7_2345678(const volatile A&&);

int test7_2345678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_7_2345678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_7_2345678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_7_2345678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_7_2345678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_7_2345678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_7_2345678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_7_2345678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test7_1234567() + test7_1234568() + test7_1234578() + test7_1234678() +
           test7_1235678() + test7_1245678() + test7_1345678() + test7_2345678();
}
