// I, Howard Hinnant, hereby place this code in the public domain.

// Test overlaod resolution among referece types

// { dg-do compile }
// { dg-options "-std=c++0x" }

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

// 6 at a time

one   sink_6_123456(               A&);
two   sink_6_123456(const          A&);
three sink_6_123456(volatile       A&);
four  sink_6_123456(const volatile A&);
five  sink_6_123456(               A&&);
six   sink_6_123456(const          A&&);

int test6_123456()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123456(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123456(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123456(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123456(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_123456(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_123456(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_6_123457(               A&);
two   sink_6_123457(const          A&);
three sink_6_123457(volatile       A&);
four  sink_6_123457(const volatile A&);
five  sink_6_123457(               A&&);
seven sink_6_123457(volatile       A&&);

int test6_123457()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123457(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123457(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123457(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123457(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_123457(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_123457(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_6_123457(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_6_123458(               A&);
two   sink_6_123458(const          A&);
three sink_6_123458(volatile       A&);
four  sink_6_123458(const volatile A&);
five  sink_6_123458(               A&&);
eight sink_6_123458(const volatile A&&);

int test6_123458()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123458(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123458(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123458(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123458(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_123458(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_123458(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_6_123458(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_6_123458(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_123467(               A&);
two   sink_6_123467(const          A&);
three sink_6_123467(volatile       A&);
four  sink_6_123467(const volatile A&);
six   sink_6_123467(const          A&&);
seven sink_6_123467(volatile       A&&);

int test6_123467()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123467(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123467(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123467(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123467(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_123467(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_123467(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_6_123468(               A&);
two   sink_6_123468(const          A&);
three sink_6_123468(volatile       A&);
four  sink_6_123468(const volatile A&);
six   sink_6_123468(const          A&&);
eight sink_6_123468(const volatile A&&);

int test6_123468()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123468(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123468(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123468(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123468(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_123468(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_6_123468(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_123468(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_6_123468(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_123478(               A&);
two   sink_6_123478(const          A&);
three sink_6_123478(volatile       A&);
four  sink_6_123478(const volatile A&);
seven sink_6_123478(volatile       A&&);
eight sink_6_123478(const volatile A&&);

int test6_123478()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123478(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123478(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123478(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123478(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_123478(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_6_123478(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_6_123478(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_123478(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_123567(               A&);
two   sink_6_123567(const          A&);
three sink_6_123567(volatile       A&);
five  sink_6_123567(               A&&);
six   sink_6_123567(const          A&&);
seven sink_6_123567(volatile       A&&);

int test6_123567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123567(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123567(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123567(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_123567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_123567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_6_123568(               A&);
two   sink_6_123568(const          A&);
three sink_6_123568(volatile       A&);
five  sink_6_123568(               A&&);
six   sink_6_123568(const          A&&);
eight sink_6_123568(const volatile A&&);

int test6_123568()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123568(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123568(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123568(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123568(cva))         == 8 * sizeof(long)> t4;
    sa<sizeof(sink_6_123568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_123568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_123568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_6_123568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_123578(               A&);
two   sink_6_123578(const          A&);
three sink_6_123578(volatile       A&);
five  sink_6_123578(               A&&);
seven sink_6_123578(volatile       A&&);
eight sink_6_123578(const volatile A&&);

int test6_123578()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123578(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123578(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123578(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123578(cva))         == 8 * sizeof(long)> t4;
    sa<sizeof(sink_6_123578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_123578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_6_123578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_123578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_123678(               A&);
two   sink_6_123678(const          A&);
three sink_6_123678(volatile       A&);
six   sink_6_123678(const          A&&);
seven sink_6_123678(volatile       A&&);
eight sink_6_123678(const volatile A&&);

int test6_123678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_123678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_123678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_123678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_123678(cva))         == 8 * sizeof(long)> t4;
    sa<sizeof(sink_6_123678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_123678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_123678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_124567(               A&);
two   sink_6_124567(const          A&);
four  sink_6_124567(const volatile A&);
five  sink_6_124567(               A&&);
six   sink_6_124567(const          A&&);
seven sink_6_124567(volatile       A&&);

int test6_124567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_124567(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_124567(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_124567(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_6_124567(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_124567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_124567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_124567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_6_124568(               A&);
two   sink_6_124568(const          A&);
four  sink_6_124568(const volatile A&);
five  sink_6_124568(               A&&);
six   sink_6_124568(const          A&&);
eight sink_6_124568(const volatile A&&);

int test6_124568()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_124568(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_124568(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_124568(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_6_124568(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_124568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_124568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_124568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_6_124568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_124578(               A&);
two   sink_6_124578(const          A&);
four  sink_6_124578(const volatile A&);
five  sink_6_124578(               A&&);
seven sink_6_124578(volatile       A&&);
eight sink_6_124578(const volatile A&&);

int test6_124578()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_124578(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_124578(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_124578(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_6_124578(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_124578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_124578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_6_124578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_124578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_124678(               A&);
two   sink_6_124678(const          A&);
four  sink_6_124678(const volatile A&);
six   sink_6_124678(const          A&&);
seven sink_6_124678(volatile       A&&);
eight sink_6_124678(const volatile A&&);

int test6_124678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_124678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_124678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_124678(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_6_124678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_124678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_124678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_124678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_125678(               A&);
two   sink_6_125678(const          A&);
five  sink_6_125678(               A&&);
six   sink_6_125678(const          A&&);
seven sink_6_125678(volatile       A&&);
eight sink_6_125678(const volatile A&&);

int test6_125678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_125678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_125678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_125678(va))          == 7 * sizeof(long)> t3;
    sa<sizeof(sink_6_125678(cva))         == 8 * sizeof(long)> t4;
    sa<sizeof(sink_6_125678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_125678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_125678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_125678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_134567(               A&);
three sink_6_134567(volatile       A&);
four  sink_6_134567(const volatile A&);
five  sink_6_134567(               A&&);
six   sink_6_134567(const          A&&);
seven sink_6_134567(volatile       A&&);

int test6_134567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_134567(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_134567(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_6_134567(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_134567(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_134567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_134567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_134567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_6_134568(               A&);
three sink_6_134568(volatile       A&);
four  sink_6_134568(const volatile A&);
five  sink_6_134568(               A&&);
six   sink_6_134568(const          A&&);
eight sink_6_134568(const volatile A&&);

int test6_134568()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_134568(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_134568(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_6_134568(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_134568(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_134568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_134568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_134568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_6_134568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_134578(               A&);
three sink_6_134578(volatile       A&);
four  sink_6_134578(const volatile A&);
five  sink_6_134578(               A&&);
seven sink_6_134578(volatile       A&&);
eight sink_6_134578(const volatile A&&);

int test6_134578()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_134578(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_134578(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_6_134578(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_134578(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_134578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_134578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_6_134578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_134578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_134678(               A&);
three sink_6_134678(volatile       A&);
four  sink_6_134678(const volatile A&);
six   sink_6_134678(const          A&&);
seven sink_6_134678(volatile       A&&);
eight sink_6_134678(const volatile A&&);

int test6_134678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_134678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_134678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_6_134678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_134678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_134678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_134678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_134678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_135678(               A&);
three sink_6_135678(volatile       A&);
five  sink_6_135678(               A&&);
six   sink_6_135678(const          A&&);
seven sink_6_135678(volatile       A&&);
eight sink_6_135678(const volatile A&&);

int test6_135678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_135678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_135678(ca))          == 6 * sizeof(long)> t2;
    sa<sizeof(sink_6_135678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_135678(cva))         == 8 * sizeof(long)> t4;
    sa<sizeof(sink_6_135678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_135678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_135678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_135678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_6_145678(               A&);
four  sink_6_145678(const volatile A&);
five  sink_6_145678(               A&&);
six   sink_6_145678(const          A&&);
seven sink_6_145678(volatile       A&&);
eight sink_6_145678(const volatile A&&);

int test6_145678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_145678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_6_145678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_6_145678(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_6_145678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_145678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_145678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_145678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_145678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_6_234567(const          A&);
three sink_6_234567(volatile       A&);
four  sink_6_234567(const volatile A&);
five  sink_6_234567(               A&&);
six   sink_6_234567(const          A&&);
seven sink_6_234567(volatile       A&&);

int test6_234567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_234567(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_234567(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_234567(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_234567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_234567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_234567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_6_234568(const          A&);
three sink_6_234568(volatile       A&);
four  sink_6_234568(const volatile A&);
five  sink_6_234568(               A&&);
six   sink_6_234568(const          A&&);
eight sink_6_234568(const volatile A&&);

int test6_234568()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_234568(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_234568(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_234568(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_234568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_234568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_234568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_6_234568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_6_234578(const          A&);
three sink_6_234578(volatile       A&);
four  sink_6_234578(const volatile A&);
five  sink_6_234578(               A&&);
seven sink_6_234578(volatile       A&&);
eight sink_6_234578(const volatile A&&);

int test6_234578()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_234578(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_234578(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_234578(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_234578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_234578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_6_234578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_234578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_6_234678(const          A&);
three sink_6_234678(volatile       A&);
four  sink_6_234678(const volatile A&);
six   sink_6_234678(const          A&&);
seven sink_6_234678(volatile       A&&);
eight sink_6_234678(const volatile A&&);

int test6_234678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_234678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_234678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_234678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_234678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_234678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_234678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_6_235678(const          A&);
three sink_6_235678(volatile       A&);
five  sink_6_235678(               A&&);
six   sink_6_235678(const          A&&);
seven sink_6_235678(volatile       A&&);
eight sink_6_235678(const volatile A&&);

int test6_235678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_235678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_235678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_235678(cva))         == 8 * sizeof(long)> t4;
    sa<sizeof(sink_6_235678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_235678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_235678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_235678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_6_245678(const          A&);
four  sink_6_245678(const volatile A&);
five  sink_6_245678(               A&&);
six   sink_6_245678(const          A&&);
seven sink_6_245678(volatile       A&&);
eight sink_6_245678(const volatile A&&);

int test6_245678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_245678(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_6_245678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_6_245678(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_6_245678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_245678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_245678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_245678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_245678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_6_345678(volatile       A&);
four  sink_6_345678(const volatile A&);
five  sink_6_345678(               A&&);
six   sink_6_345678(const          A&&);
seven sink_6_345678(volatile       A&&);
eight sink_6_345678(const volatile A&&);

int test6_345678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sa<sizeof(sink_6_345678(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_6_345678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_6_345678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_6_345678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_6_345678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_6_345678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_6_345678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_6_345678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test6_123456() + test6_123457() + test6_123458() + test6_123467() +
           test6_123468() + test6_123478() + test6_123567() + test6_123568() +
           test6_123578() + test6_123678() + test6_124567() + test6_124568() +
           test6_124578() + test6_124678() + test6_125678() + test6_134567() +
           test6_134568() + test6_134578() + test6_134678() + test6_135678() +
           test6_145678() + test6_234567() + test6_234568() + test6_234578() +
           test6_234678() + test6_235678() + test6_245678() + test6_345678();
}
