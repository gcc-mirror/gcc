// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile }
// { dg-options "-std=c++0x" }

template <bool> struct sa;
template <> struct sa<true> {};

struct one   {char x[1];};
struct two   {char x[2];};
struct three {char x[3];};
struct four  {char x[4];};
struct five  {char x[5];};
struct six   {char x[6];};
struct seven {char x[7];};
struct eight {char x[8];};

struct A
{
    A();
    A(const volatile A&&);
};

               A    source();
const          A  c_source();
      volatile A  v_source();
const volatile A cv_source();

// 1 at a time

one   sink_1_1(               A&);

int test1_1()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_1(a))           == 1> t1;
    return 0;
}

two   sink_1_2(const          A&);

int test1_2()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_2(a))           == 2> t1;
    sa<sizeof(sink_1_2(ca))          == 2> t2;
    sa<sizeof(sink_1_2(source()))    == 2> t5;
    sa<sizeof(sink_1_2(c_source()))  == 2> t6;
    return 0;
}

three sink_1_3(volatile       A&);

int test1_3()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_3(a))           == 3> t1;
    sa<sizeof(sink_1_3(va))          == 3> t3;
    return 0;
}

four  sink_1_4(const volatile A&);

int test1_4()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_4(a))           == 4> t1;
    sa<sizeof(sink_1_4(ca))          == 4> t2;
    sa<sizeof(sink_1_4(va))          == 4> t3;
    sa<sizeof(sink_1_4(cva))         == 4> t4;
    return 0;
}

five  sink_1_5(               A&&);

int test1_5()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_5(source()))    == 5> t5;
    return 0;
}

six   sink_1_6(const          A&&);

int test1_6()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_6(source()))    == 6> t5;
    sa<sizeof(sink_1_6(c_source()))  == 6> t6;
    return 0;
}

seven sink_1_7(volatile       A&&);

int test1_7()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_7(source()))    == 7> t5;
    sa<sizeof(sink_1_7(v_source()))  == 7> t7;
    return 0;
}

eight sink_1_8(const volatile A&&);

int test1_8()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_1_8(source()))    == 8> t5;
    sa<sizeof(sink_1_8(c_source()))  == 8> t6;
    sa<sizeof(sink_1_8(v_source()))  == 8> t7;
    sa<sizeof(sink_1_8(cv_source())) == 8> t8;
    return 0;
}

int main()
{
    return test1_1() + test1_2() + test1_3() + test1_4() +
           test1_5() + test1_6() + test1_7() + test1_8();
}
