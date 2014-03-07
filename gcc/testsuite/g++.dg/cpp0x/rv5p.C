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

// 5 at a time

one   sink_5_12345(               A&);
two   sink_5_12345(const          A&);
three sink_5_12345(volatile       A&);
four  sink_5_12345(const volatile A&);
five  sink_5_12345(               A&&);

int test5_12345()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12345(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12345(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12345(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12345(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12345(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12345(c_source()))  == 2* sizeof(long)> t6;
    return 0;
}

one   sink_5_12346(               A&);
two   sink_5_12346(const          A&);
three sink_5_12346(volatile       A&);
four  sink_5_12346(const volatile A&);
six   sink_5_12346(const          A&&);

int test5_12346()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12346(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12346(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12346(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12346(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12346(source()))    == 6* sizeof(long)> t5;
    sa<sizeof(sink_5_12346(c_source()))  == 6* sizeof(long)> t6;
    return 0;
}

one   sink_5_12347(               A&);
two   sink_5_12347(const          A&);
three sink_5_12347(volatile       A&);
four  sink_5_12347(const volatile A&);
seven sink_5_12347(volatile       A&&);

int test5_12347()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12347(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12347(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12347(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12347(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12347(source()))    == 7* sizeof(long)> t5;
    sa<sizeof(sink_5_12347(c_source()))  == 2* sizeof(long)> t6;
    sa<sizeof(sink_5_12347(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_12348(               A&);
two   sink_5_12348(const          A&);
three sink_5_12348(volatile       A&);
four  sink_5_12348(const volatile A&);
eight sink_5_12348(const volatile A&&);

int test5_12348()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12348(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12348(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12348(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12348(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12348(source()))    == 8* sizeof(long)> t5;
    sa<sizeof(sink_5_12348(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_12348(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_12348(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12356(               A&);
two   sink_5_12356(const          A&);
three sink_5_12356(volatile       A&);
five  sink_5_12356(               A&&);
six   sink_5_12356(const          A&&);

int test5_12356()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12356(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12356(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12356(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12356(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12356(c_source()))  == 6* sizeof(long)> t6;
    return 0;
}

one   sink_5_12357(               A&);
two   sink_5_12357(const          A&);
three sink_5_12357(volatile       A&);
five  sink_5_12357(               A&&);
seven sink_5_12357(volatile       A&&);

int test5_12357()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12357(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12357(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12357(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12357(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12357(c_source()))  == 2* sizeof(long)> t6;
    sa<sizeof(sink_5_12357(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_12358(               A&);
two   sink_5_12358(const          A&);
three sink_5_12358(volatile       A&);
five  sink_5_12358(               A&&);
eight sink_5_12358(const volatile A&&);

int test5_12358()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12358(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12358(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12358(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12358(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12358(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_12358(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_12358(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12367(               A&);
two   sink_5_12367(const          A&);
three sink_5_12367(volatile       A&);
six   sink_5_12367(const          A&&);
seven sink_5_12367(volatile       A&&);

int test5_12367()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12367(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12367(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12367(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12367(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12367(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_12368(               A&);
two   sink_5_12368(const          A&);
three sink_5_12368(volatile       A&);
six   sink_5_12368(const          A&&);
eight sink_5_12368(const volatile A&&);

int test5_12368()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12368(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12368(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12368(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12368(source()))    == 6* sizeof(long)> t5;
    sa<sizeof(sink_5_12368(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12368(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_12368(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12378(               A&);
two   sink_5_12378(const          A&);
three sink_5_12378(volatile       A&);
seven sink_5_12378(volatile       A&&);
eight sink_5_12378(const volatile A&&);

int test5_12378()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12378(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12378(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12378(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_12378(source()))    == 7* sizeof(long)> t5;
    sa<sizeof(sink_5_12378(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_12378(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_12378(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12456(               A&);
two   sink_5_12456(const          A&);
four  sink_5_12456(const volatile A&);
five  sink_5_12456(               A&&);
six   sink_5_12456(const          A&&);

int test5_12456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12456(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12456(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12456(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_12456(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12456(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12456(c_source()))  == 6* sizeof(long)> t6;
    return 0;
}

one   sink_5_12457(               A&);
two   sink_5_12457(const          A&);
four  sink_5_12457(const volatile A&);
five  sink_5_12457(               A&&);
seven sink_5_12457(volatile       A&&);

int test5_12457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12457(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12457(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12457(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_12457(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12457(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12457(c_source()))  == 2* sizeof(long)> t6;
    sa<sizeof(sink_5_12457(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_12458(               A&);
two   sink_5_12458(const          A&);
four  sink_5_12458(const volatile A&);
five  sink_5_12458(               A&&);
eight sink_5_12458(const volatile A&&);

int test5_12458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12458(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12458(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12458(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_12458(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12458(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12458(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_12458(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_12458(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12467(               A&);
two   sink_5_12467(const          A&);
four  sink_5_12467(const volatile A&);
six   sink_5_12467(const          A&&);
seven sink_5_12467(volatile       A&&);

int test5_12467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12467(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12467(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12467(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_12467(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12467(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12467(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_12468(               A&);
two   sink_5_12468(const          A&);
four  sink_5_12468(const volatile A&);
six   sink_5_12468(const          A&&);
eight sink_5_12468(const volatile A&&);

int test5_12468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12468(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12468(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12468(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_12468(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12468(source()))    == 6* sizeof(long)> t5;
    sa<sizeof(sink_5_12468(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12468(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_12468(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12478(               A&);
two   sink_5_12478(const          A&);
four  sink_5_12478(const volatile A&);
seven sink_5_12478(volatile       A&&);
eight sink_5_12478(const volatile A&&);

int test5_12478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12478(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12478(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12478(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_12478(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_12478(source()))    == 7* sizeof(long)> t5;
    sa<sizeof(sink_5_12478(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_12478(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_12478(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12567(               A&);
two   sink_5_12567(const          A&);
five  sink_5_12567(               A&&);
six   sink_5_12567(const          A&&);
seven sink_5_12567(volatile       A&&);

int test5_12567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12567(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12567(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12567(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12567(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12567(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_12568(               A&);
two   sink_5_12568(const          A&);
five  sink_5_12568(               A&&);
six   sink_5_12568(const          A&&);
eight sink_5_12568(const volatile A&&);

int test5_12568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12568(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12568(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12568(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12568(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12568(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_12568(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12578(               A&);
two   sink_5_12578(const          A&);
five  sink_5_12578(               A&&);
seven sink_5_12578(volatile       A&&);
eight sink_5_12578(const volatile A&&);

int test5_12578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12578(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12578(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12578(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_12578(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_12578(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_12578(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_12678(               A&);
two   sink_5_12678(const          A&);
six   sink_5_12678(const          A&&);
seven sink_5_12678(volatile       A&&);
eight sink_5_12678(const volatile A&&);

int test5_12678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_12678(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_12678(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_12678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_12678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_12678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_13456(               A&);
three sink_5_13456(volatile       A&);
four  sink_5_13456(const volatile A&);
five  sink_5_13456(               A&&);
six   sink_5_13456(const          A&&);

int test5_13456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13456(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13456(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_13456(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13456(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_13456(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_13456(c_source()))  == 6* sizeof(long)> t6;
    return 0;
}

one   sink_5_13457(               A&);
three sink_5_13457(volatile       A&);
four  sink_5_13457(const volatile A&);
five  sink_5_13457(               A&&);
seven sink_5_13457(volatile       A&&);

int test5_13457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13457(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13457(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_13457(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13457(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_13457(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_13457(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_13458(               A&);
three sink_5_13458(volatile       A&);
four  sink_5_13458(const volatile A&);
five  sink_5_13458(               A&&);
eight sink_5_13458(const volatile A&&);

int test5_13458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13458(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13458(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_13458(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13458(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_13458(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_13458(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_13458(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_13458(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_13467(               A&);
three sink_5_13467(volatile       A&);
four  sink_5_13467(const volatile A&);
six   sink_5_13467(const          A&&);
seven sink_5_13467(volatile       A&&);

int test5_13467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13467(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13467(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_13467(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13467(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_13467(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_13467(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_13468(               A&);
three sink_5_13468(volatile       A&);
four  sink_5_13468(const volatile A&);
six   sink_5_13468(const          A&&);
eight sink_5_13468(const volatile A&&);

int test5_13468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13468(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13468(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_13468(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13468(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_13468(source()))    == 6* sizeof(long)> t5;
    sa<sizeof(sink_5_13468(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_13468(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_13468(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_13478(               A&);
three sink_5_13478(volatile       A&);
four  sink_5_13478(const volatile A&);
seven sink_5_13478(volatile       A&&);
eight sink_5_13478(const volatile A&&);

int test5_13478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13478(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13478(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_13478(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13478(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_13478(source()))    == 7* sizeof(long)> t5;
    sa<sizeof(sink_5_13478(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_13478(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_13478(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_13567(               A&);
three sink_5_13567(volatile       A&);
five  sink_5_13567(               A&&);
six   sink_5_13567(const          A&&);
seven sink_5_13567(volatile       A&&);

int test5_13567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13567(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13567(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13567(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_13567(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_13567(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_13568(               A&);
three sink_5_13568(volatile       A&);
five  sink_5_13568(               A&&);
six   sink_5_13568(const          A&&);
eight sink_5_13568(const volatile A&&);

int test5_13568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13568(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13568(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13568(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_13568(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_13568(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_13568(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_13578(               A&);
three sink_5_13578(volatile       A&);
five  sink_5_13578(               A&&);
seven sink_5_13578(volatile       A&&);
eight sink_5_13578(const volatile A&&);

int test5_13578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13578(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13578(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13578(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_13578(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_13578(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_13578(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_13678(               A&);
three sink_5_13678(volatile       A&);
six   sink_5_13678(const          A&&);
seven sink_5_13678(volatile       A&&);
eight sink_5_13678(const volatile A&&);

int test5_13678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_13678(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_13678(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_13678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_13678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_13678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_14567(               A&);
four  sink_5_14567(const volatile A&);
five  sink_5_14567(               A&&);
six   sink_5_14567(const          A&&);
seven sink_5_14567(volatile       A&&);

int test5_14567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_14567(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_14567(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_14567(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_14567(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_14567(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_14567(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_14567(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

one   sink_5_14568(               A&);
four  sink_5_14568(const volatile A&);
five  sink_5_14568(               A&&);
six   sink_5_14568(const          A&&);
eight sink_5_14568(const volatile A&&);

int test5_14568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_14568(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_14568(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_14568(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_14568(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_14568(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_14568(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_14568(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_14568(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_14578(               A&);
four  sink_5_14578(const volatile A&);
five  sink_5_14578(               A&&);
seven sink_5_14578(volatile       A&&);
eight sink_5_14578(const volatile A&&);

int test5_14578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_14578(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_14578(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_14578(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_14578(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_14578(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_14578(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_14578(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_14578(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_14678(               A&);
four  sink_5_14678(const volatile A&);
six   sink_5_14678(const          A&&);
seven sink_5_14678(volatile       A&&);
eight sink_5_14678(const volatile A&&);

int test5_14678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_14678(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_14678(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_14678(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_14678(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_14678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_14678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_14678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

one   sink_5_15678(               A&);
five  sink_5_15678(               A&&);
six   sink_5_15678(const          A&&);
seven sink_5_15678(volatile       A&&);
eight sink_5_15678(const volatile A&&);

int test5_15678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_15678(a))           == 1* sizeof(long)> t1;
    sa<sizeof(sink_5_15678(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_15678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_15678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_15678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_23456(const          A&);
three sink_5_23456(volatile       A&);
four  sink_5_23456(const volatile A&);
five  sink_5_23456(               A&&);
six   sink_5_23456(const          A&&);

int test5_23456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23456(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23456(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23456(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_23456(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_23456(c_source()))  == 6* sizeof(long)> t6;
    return 0;
}

two   sink_5_23457(const          A&);
three sink_5_23457(volatile       A&);
four  sink_5_23457(const volatile A&);
five  sink_5_23457(               A&&);
seven sink_5_23457(volatile       A&&);

int test5_23457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23457(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23457(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23457(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_23457(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_23457(c_source()))  == 2* sizeof(long)> t6;
    sa<sizeof(sink_5_23457(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

two   sink_5_23458(const          A&);
three sink_5_23458(volatile       A&);
four  sink_5_23458(const volatile A&);
five  sink_5_23458(               A&&);
eight sink_5_23458(const volatile A&&);

int test5_23458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23458(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23458(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23458(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_23458(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_23458(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_23458(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_23458(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_23467(const          A&);
three sink_5_23467(volatile       A&);
four  sink_5_23467(const volatile A&);
six   sink_5_23467(const          A&&);
seven sink_5_23467(volatile       A&&);

int test5_23467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23467(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23467(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23467(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_23467(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_23467(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

two   sink_5_23468(const          A&);
three sink_5_23468(volatile       A&);
four  sink_5_23468(const volatile A&);
six   sink_5_23468(const          A&&);
eight sink_5_23468(const volatile A&&);

int test5_23468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23468(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23468(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23468(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_23468(source()))    == 6* sizeof(long)> t5;
    sa<sizeof(sink_5_23468(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_23468(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_23468(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_23478(const          A&);
three sink_5_23478(volatile       A&);
four  sink_5_23478(const volatile A&);
seven sink_5_23478(volatile       A&&);
eight sink_5_23478(const volatile A&&);

int test5_23478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23478(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23478(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23478(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_23478(source()))    == 7* sizeof(long)> t5;
    sa<sizeof(sink_5_23478(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_23478(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_23478(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_23567(const          A&);
three sink_5_23567(volatile       A&);
five  sink_5_23567(               A&&);
six   sink_5_23567(const          A&&);
seven sink_5_23567(volatile       A&&);

int test5_23567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23567(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23567(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23567(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_23567(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_23567(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

two   sink_5_23568(const          A&);
three sink_5_23568(volatile       A&);
five  sink_5_23568(               A&&);
six   sink_5_23568(const          A&&);
eight sink_5_23568(const volatile A&&);

int test5_23568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23568(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23568(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23568(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_23568(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_23568(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_23568(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_23578(const          A&);
three sink_5_23578(volatile       A&);
five  sink_5_23578(               A&&);
seven sink_5_23578(volatile       A&&);
eight sink_5_23578(const volatile A&&);

int test5_23578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23578(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23578(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23578(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_23578(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_23578(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_23578(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_23678(const          A&);
three sink_5_23678(volatile       A&);
six   sink_5_23678(const          A&&);
seven sink_5_23678(volatile       A&&);
eight sink_5_23678(const volatile A&&);

int test5_23678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_23678(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_23678(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_23678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_23678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_23678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_24567(const          A&);
four  sink_5_24567(const volatile A&);
five  sink_5_24567(               A&&);
six   sink_5_24567(const          A&&);
seven sink_5_24567(volatile       A&&);

int test5_24567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_24567(a))           == 2* sizeof(long)> t1;
    sa<sizeof(sink_5_24567(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_24567(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_24567(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_24567(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_24567(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_24567(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

two   sink_5_24568(const          A&);
four  sink_5_24568(const volatile A&);
five  sink_5_24568(               A&&);
six   sink_5_24568(const          A&&);
eight sink_5_24568(const volatile A&&);

int test5_24568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_24568(a))           == 2* sizeof(long)> t1;
    sa<sizeof(sink_5_24568(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_24568(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_24568(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_24568(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_24568(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_24568(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_24568(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_24578(const          A&);
four  sink_5_24578(const volatile A&);
five  sink_5_24578(               A&&);
seven sink_5_24578(volatile       A&&);
eight sink_5_24578(const volatile A&&);

int test5_24578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_24578(a))           == 2* sizeof(long)> t1;
    sa<sizeof(sink_5_24578(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_24578(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_24578(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_24578(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_24578(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_24578(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_24578(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_24678(const          A&);
four  sink_5_24678(const volatile A&);
six   sink_5_24678(const          A&&);
seven sink_5_24678(volatile       A&&);
eight sink_5_24678(const volatile A&&);

int test5_24678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_24678(a))           == 2* sizeof(long)> t1;
    sa<sizeof(sink_5_24678(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_24678(va))          == 4* sizeof(long)> t3;
    sa<sizeof(sink_5_24678(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_24678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_24678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_24678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

two   sink_5_25678(const          A&);
five  sink_5_25678(               A&&);
six   sink_5_25678(const          A&&);
seven sink_5_25678(volatile       A&&);
eight sink_5_25678(const volatile A&&);

int test5_25678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_25678(a))           == 2* sizeof(long)> t1;
    sa<sizeof(sink_5_25678(ca))          == 2* sizeof(long)> t2;
    sa<sizeof(sink_5_25678(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_25678(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_25678(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_25678(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

three sink_5_34567(volatile       A&);
four  sink_5_34567(const volatile A&);
five  sink_5_34567(               A&&);
six   sink_5_34567(const          A&&);
seven sink_5_34567(volatile       A&&);

int test5_34567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_34567(a))           == 3* sizeof(long)> t1;
    sa<sizeof(sink_5_34567(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_34567(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_34567(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_34567(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_34567(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_34567(v_source()))  == 7* sizeof(long)> t7;
    return 0;
}

three sink_5_34568(volatile       A&);
four  sink_5_34568(const volatile A&);
five  sink_5_34568(               A&&);
six   sink_5_34568(const          A&&);
eight sink_5_34568(const volatile A&&);

int test5_34568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_34568(a))           == 3* sizeof(long)> t1;
    sa<sizeof(sink_5_34568(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_34568(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_34568(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_34568(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_34568(c_source()))  == 6* sizeof(long)> t6;
    sa<sizeof(sink_5_34568(v_source()))  == 8* sizeof(long)> t7;
    sa<sizeof(sink_5_34568(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

three sink_5_34578(volatile       A&);
four  sink_5_34578(const volatile A&);
five  sink_5_34578(               A&&);
seven sink_5_34578(volatile       A&&);
eight sink_5_34578(const volatile A&&);

int test5_34578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_34578(a))           == 3* sizeof(long)> t1;
    sa<sizeof(sink_5_34578(ca))          == 4* sizeof(long)> t2;
    sa<sizeof(sink_5_34578(va))          == 3* sizeof(long)> t3;
    sa<sizeof(sink_5_34578(cva))         == 4* sizeof(long)> t4;
    sa<sizeof(sink_5_34578(source()))    == 5* sizeof(long)> t5;
    sa<sizeof(sink_5_34578(c_source()))  == 8* sizeof(long)> t6;
    sa<sizeof(sink_5_34578(v_source()))  == 7* sizeof(long)> t7;
    sa<sizeof(sink_5_34578(cv_source())) == 8* sizeof(long)> t8;
    return 0;
}

three sink_5_34678(volatile       A&);
four  sink_5_34678(const volatile A&);
six   sink_5_34678(const          A&&);
seven sink_5_34678(volatile       A&&);
eight sink_5_34678(const volatile A&&);

int test5_34678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_34678(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_5_34678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_5_34678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_5_34678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_5_34678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_5_34678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_5_34678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_5_35678(volatile       A&);
five  sink_5_35678(               A&&);
six   sink_5_35678(const          A&&);
seven sink_5_35678(volatile       A&&);
eight sink_5_35678(const volatile A&&);

int test5_35678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_35678(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_5_35678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_5_35678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_5_35678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_5_35678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_5_35678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_5_45678(const volatile A&);
five  sink_5_45678(               A&&);
six   sink_5_45678(const          A&&);
seven sink_5_45678(volatile       A&&);
eight sink_5_45678(const volatile A&&);

int test5_45678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_5_45678(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_5_45678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_5_45678(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_5_45678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_5_45678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_5_45678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_5_45678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_5_45678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test5_12345() + test5_12346() + test5_12347() + test5_12348() +
           test5_12356() + test5_12357() + test5_12358() + test5_12367() +
           test5_12368() + test5_12378() + test5_12456() + test5_12457() +
           test5_12458() + test5_12467() + test5_12468() + test5_12478() +
           test5_12567() + test5_12568() + test5_12578() + test5_12678() +
           test5_13456() + test5_13457() + test5_13458() + test5_13467() +
           test5_13468() + test5_13478() + test5_13567() + test5_13568() +
           test5_13578() + test5_13678() + test5_14567() + test5_14568() +
           test5_14578() + test5_14678() + test5_15678() + test5_23456() +
           test5_23457() + test5_23458() + test5_23467() + test5_23468() +
           test5_23478() + test5_23567() + test5_23568() + test5_23578() +
           test5_23678() + test5_24567() + test5_24568() + test5_24578() +
           test5_24678() + test5_25678() + test5_34567() + test5_34568() +
           test5_34578() + test5_34678() + test5_35678() + test5_45678();
}
