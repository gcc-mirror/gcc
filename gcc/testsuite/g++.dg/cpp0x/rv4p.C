// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile { target c++11 } }
// { dg-skip-if "packed attribute missing for struct one/three/five/seven" { "epiphany-*-*" } }

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

// 4 at a time

one   sink_4_1234(               A&);
two   sink_4_1234(const          A&);
three sink_4_1234(volatile       A&);
four  sink_4_1234(const volatile A&);

int test4_1234()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1234(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1234(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1234(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1234(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1234(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_4_1234(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1235(               A&);
two   sink_4_1235(const          A&);
three sink_4_1235(volatile       A&);
five  sink_4_1235(               A&&);

int test4_1235()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1235(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1235(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1235(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1235(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1235(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1236(               A&);
two   sink_4_1236(const          A&);
three sink_4_1236(volatile       A&);
six   sink_4_1236(const          A&&);

int test4_1236()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1236(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1236(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1236(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1236(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_1236(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1237(               A&);
two   sink_4_1237(const          A&);
three sink_4_1237(volatile       A&);
seven sink_4_1237(volatile       A&&);

int test4_1237()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1237(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1237(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1237(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1237(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_1237(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_4_1237(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1238(               A&);
two   sink_4_1238(const          A&);
three sink_4_1238(volatile       A&);
eight sink_4_1238(const volatile A&&);

int test4_1238()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1238(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1238(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1238(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1238(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_4_1238(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1238(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1238(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1245(               A&);
two   sink_4_1245(const          A&);
four  sink_4_1245(const volatile A&);
five  sink_4_1245(               A&&);

int test4_1245()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1245(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1245(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1245(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1245(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1245(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1245(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1246(               A&);
two   sink_4_1246(const          A&);
four  sink_4_1246(const volatile A&);
six   sink_4_1246(const          A&&);

int test4_1246()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1246(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1246(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1246(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1246(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1246(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_1246(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1247(               A&);
two   sink_4_1247(const          A&);
four  sink_4_1247(const volatile A&);
seven sink_4_1247(volatile       A&&);

int test4_1247()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1247(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1247(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1247(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1247(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1247(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_1247(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_4_1247(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1248(               A&);
two   sink_4_1248(const          A&);
four  sink_4_1248(const volatile A&);
eight sink_4_1248(const volatile A&&);

int test4_1248()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1248(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1248(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1248(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1248(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1248(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_4_1248(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1248(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1248(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1256(               A&);
two   sink_4_1256(const          A&);
five  sink_4_1256(               A&&);
six   sink_4_1256(const          A&&);

int test4_1256()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1256(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1256(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1256(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1256(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1257(               A&);
two   sink_4_1257(const          A&);
five  sink_4_1257(               A&&);
seven sink_4_1257(volatile       A&&);

int test4_1257()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1257(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1257(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1257(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1257(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_4_1257(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1258(               A&);
two   sink_4_1258(const          A&);
five  sink_4_1258(               A&&);
eight sink_4_1258(const volatile A&&);

int test4_1258()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1258(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1258(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1258(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1258(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1258(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1258(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1267(               A&);
two   sink_4_1267(const          A&);
six   sink_4_1267(const          A&&);
seven sink_4_1267(volatile       A&&);

int test4_1267()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1267(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1267(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1267(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1267(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1268(               A&);
two   sink_4_1268(const          A&);
six   sink_4_1268(const          A&&);
eight sink_4_1268(const volatile A&&);

int test4_1268()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1268(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1268(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1268(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_1268(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1268(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1268(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1278(               A&);
two   sink_4_1278(const          A&);
seven sink_4_1278(volatile       A&&);
eight sink_4_1278(const volatile A&&);

int test4_1278()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1278(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1278(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_1278(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_1278(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1278(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_1278(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1345(               A&);
three sink_4_1345(volatile       A&);
four  sink_4_1345(const volatile A&);
five  sink_4_1345(               A&&);

int test4_1345()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1345(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1345(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1345(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1345(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1345(source()))    == 5 * sizeof(long)> t5;
    return 0;
}

one   sink_4_1346(               A&);
three sink_4_1346(volatile       A&);
four  sink_4_1346(const volatile A&);
six   sink_4_1346(const          A&&);

int test4_1346()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1346(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1346(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1346(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1346(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1346(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_1346(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1347(               A&);
three sink_4_1347(volatile       A&);
four  sink_4_1347(const volatile A&);
seven sink_4_1347(volatile       A&&);

int test4_1347()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1347(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1347(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1347(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1347(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1347(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_1347(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1348(               A&);
three sink_4_1348(volatile       A&);
four  sink_4_1348(const volatile A&);
eight sink_4_1348(const volatile A&&);

int test4_1348()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1348(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1348(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1348(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1348(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1348(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_4_1348(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1348(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1348(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1356(               A&);
three sink_4_1356(volatile       A&);
five  sink_4_1356(               A&&);
six   sink_4_1356(const          A&&);

int test4_1356()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1356(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1356(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1356(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1356(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1357(               A&);
three sink_4_1357(volatile       A&);
five  sink_4_1357(               A&&);
seven sink_4_1357(volatile       A&&);

int test4_1357()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1357(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1357(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1357(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1357(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1358(               A&);
three sink_4_1358(volatile       A&);
five  sink_4_1358(               A&&);
eight sink_4_1358(const volatile A&&);

int test4_1358()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1358(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1358(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1358(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1358(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1358(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1358(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1367(               A&);
three sink_4_1367(volatile       A&);
six   sink_4_1367(const          A&&);
seven sink_4_1367(volatile       A&&);

int test4_1367()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1367(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1367(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1367(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1367(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1368(               A&);
three sink_4_1368(volatile       A&);
six   sink_4_1368(const          A&&);
eight sink_4_1368(const volatile A&&);

int test4_1368()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1368(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1368(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1368(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_1368(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1368(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1368(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1378(               A&);
three sink_4_1378(volatile       A&);
seven sink_4_1378(volatile       A&&);
eight sink_4_1378(const volatile A&&);

int test4_1378()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1378(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1378(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_1378(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_1378(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1378(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_1378(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1456(               A&);
four  sink_4_1456(const volatile A&);
five  sink_4_1456(               A&&);
six   sink_4_1456(const          A&&);

int test4_1456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1456(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1456(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1456(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1456(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1456(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1456(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_4_1457(               A&);
four  sink_4_1457(const volatile A&);
five  sink_4_1457(               A&&);
seven sink_4_1457(volatile       A&&);

int test4_1457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1457(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1457(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1457(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1457(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1457(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1457(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1458(               A&);
four  sink_4_1458(const volatile A&);
five  sink_4_1458(               A&&);
eight sink_4_1458(const volatile A&&);

int test4_1458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1458(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1458(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1458(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1458(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1458(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1458(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1458(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1458(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1467(               A&);
four  sink_4_1467(const volatile A&);
six   sink_4_1467(const          A&&);
seven sink_4_1467(volatile       A&&);

int test4_1467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1467(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1467(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1467(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1467(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1467(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1467(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1468(               A&);
four  sink_4_1468(const volatile A&);
six   sink_4_1468(const          A&&);
eight sink_4_1468(const volatile A&&);

int test4_1468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1468(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1468(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1468(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1468(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1468(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_1468(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1468(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1468(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1478(               A&);
four  sink_4_1478(const volatile A&);
seven sink_4_1478(volatile       A&&);
eight sink_4_1478(const volatile A&&);

int test4_1478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1478(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1478(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_1478(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_1478(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_1478(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_1478(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1478(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_1478(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1567(               A&);
five  sink_4_1567(               A&&);
six   sink_4_1567(const          A&&);
seven sink_4_1567(volatile       A&&);

int test4_1567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1567(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_4_1568(               A&);
five  sink_4_1568(               A&&);
six   sink_4_1568(const          A&&);
eight sink_4_1568(const volatile A&&);

int test4_1568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1568(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_1568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1578(               A&);
five  sink_4_1578(               A&&);
seven sink_4_1578(volatile       A&&);
eight sink_4_1578(const volatile A&&);

int test4_1578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1578(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_1578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_1578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_1578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_4_1678(               A&);
six   sink_4_1678(const          A&&);
seven sink_4_1678(volatile       A&&);
eight sink_4_1678(const volatile A&&);

int test4_1678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_1678(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_4_1678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_1678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_1678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2345(const          A&);
three sink_4_2345(volatile       A&);
four  sink_4_2345(const volatile A&);
five  sink_4_2345(               A&&);

int test4_2345()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2345(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2345(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2345(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2345(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2345(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

two   sink_4_2346(const          A&);
three sink_4_2346(volatile       A&);
four  sink_4_2346(const volatile A&);
six   sink_4_2346(const          A&&);

int test4_2346()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2346(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2346(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2346(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2346(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_2346(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_4_2347(const          A&);
three sink_4_2347(volatile       A&);
four  sink_4_2347(const volatile A&);
seven sink_4_2347(volatile       A&&);

int test4_2347()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2347(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2347(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2347(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2347(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_2347(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_4_2347(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_4_2348(const          A&);
three sink_4_2348(volatile       A&);
four  sink_4_2348(const volatile A&);
eight sink_4_2348(const volatile A&&);

int test4_2348()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2348(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2348(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2348(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2348(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_4_2348(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_2348(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_2348(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2356(const          A&);
three sink_4_2356(volatile       A&);
five  sink_4_2356(               A&&);
six   sink_4_2356(const          A&&);

int test4_2356()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2356(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2356(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2356(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2356(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_4_2357(const          A&);
three sink_4_2357(volatile       A&);
five  sink_4_2357(               A&&);
seven sink_4_2357(volatile       A&&);

int test4_2357()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2357(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2357(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2357(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2357(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_4_2357(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_4_2358(const          A&);
three sink_4_2358(volatile       A&);
five  sink_4_2358(               A&&);
eight sink_4_2358(const volatile A&&);

int test4_2358()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2358(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2358(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2358(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2358(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_2358(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_2358(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2367(const          A&);
three sink_4_2367(volatile       A&);
six   sink_4_2367(const          A&&);
seven sink_4_2367(volatile       A&&);

int test4_2367()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2367(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2367(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2367(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2367(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_4_2368(const          A&);
three sink_4_2368(volatile       A&);
six   sink_4_2368(const          A&&);
eight sink_4_2368(const volatile A&&);

int test4_2368()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2368(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2368(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2368(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_2368(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2368(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_2368(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2378(const          A&);
three sink_4_2378(volatile       A&);
seven sink_4_2378(volatile       A&&);
eight sink_4_2378(const volatile A&&);

int test4_2378()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2378(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2378(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_2378(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_2378(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_2378(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_2378(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2456(const          A&);
four  sink_4_2456(const volatile A&);
five  sink_4_2456(               A&&);
six   sink_4_2456(const          A&&);

int test4_2456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2456(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2456(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2456(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_2456(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2456(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2456(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_4_2457(const          A&);
four  sink_4_2457(const volatile A&);
five  sink_4_2457(               A&&);
seven sink_4_2457(volatile       A&&);

int test4_2457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2457(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2457(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2457(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_2457(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2457(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2457(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_4_2457(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_4_2458(const          A&);
four  sink_4_2458(const volatile A&);
five  sink_4_2458(               A&&);
eight sink_4_2458(const volatile A&&);

int test4_2458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2458(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2458(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2458(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_2458(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2458(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2458(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_2458(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_2458(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2467(const          A&);
four  sink_4_2467(const volatile A&);
six   sink_4_2467(const          A&&);
seven sink_4_2467(volatile       A&&);

int test4_2467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2467(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2467(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2467(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_2467(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2467(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2467(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_4_2468(const          A&);
four  sink_4_2468(const volatile A&);
six   sink_4_2468(const          A&&);
eight sink_4_2468(const volatile A&&);

int test4_2468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2468(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2468(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2468(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_2468(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2468(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_2468(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2468(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_2468(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2478(const          A&);
four  sink_4_2478(const volatile A&);
seven sink_4_2478(volatile       A&&);
eight sink_4_2478(const volatile A&&);

int test4_2478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2478(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2478(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2478(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_2478(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_2478(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_2478(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_2478(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_2478(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2567(const          A&);
five  sink_4_2567(               A&&);
six   sink_4_2567(const          A&&);
seven sink_4_2567(volatile       A&&);

int test4_2567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2567(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2567(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_4_2568(const          A&);
five  sink_4_2568(               A&&);
six   sink_4_2568(const          A&&);
eight sink_4_2568(const volatile A&&);

int test4_2568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2568(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2568(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_2568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2578(const          A&);
five  sink_4_2578(               A&&);
seven sink_4_2578(volatile       A&&);
eight sink_4_2578(const volatile A&&);

int test4_2578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2578(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2578(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_2578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_2578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_2578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_4_2678(const          A&);
six   sink_4_2678(const          A&&);
seven sink_4_2678(volatile       A&&);
eight sink_4_2678(const volatile A&&);

int test4_2678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_2678(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_4_2678(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_4_2678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_2678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_2678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_4_3456(volatile       A&);
four  sink_4_3456(const volatile A&);
five  sink_4_3456(               A&&);
six   sink_4_3456(const          A&&);

int test4_3456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3456(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3456(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_3456(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3456(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_3456(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_3456(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

three sink_4_3457(volatile       A&);
four  sink_4_3457(const volatile A&);
five  sink_4_3457(               A&&);
seven sink_4_3457(volatile       A&&);

int test4_3457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3457(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3457(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_3457(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3457(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_3457(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_3457(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_4_3458(volatile       A&);
four  sink_4_3458(const volatile A&);
five  sink_4_3458(               A&&);
eight sink_4_3458(const volatile A&&);

int test4_3458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3458(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3458(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_3458(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3458(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_3458(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_3458(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_3458(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_3458(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_4_3467(volatile       A&);
four  sink_4_3467(const volatile A&);
six   sink_4_3467(const          A&&);
seven sink_4_3467(volatile       A&&);

int test4_3467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3467(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3467(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_3467(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3467(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_3467(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_3467(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_4_3468(volatile       A&);
four  sink_4_3468(const volatile A&);
six   sink_4_3468(const          A&&);
eight sink_4_3468(const volatile A&&);

int test4_3468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3468(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3468(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_3468(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3468(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_3468(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_4_3468(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_3468(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_3468(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_4_3478(volatile       A&);
four  sink_4_3478(const volatile A&);
seven sink_4_3478(volatile       A&&);
eight sink_4_3478(const volatile A&&);

int test4_3478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3478(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3478(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_3478(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3478(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_3478(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_4_3478(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_3478(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_3478(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_4_3567(volatile       A&);
five  sink_4_3567(               A&&);
six   sink_4_3567(const          A&&);
seven sink_4_3567(volatile       A&&);

int test4_3567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3567(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3567(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_3567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_3567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_4_3568(volatile       A&);
five  sink_4_3568(               A&&);
six   sink_4_3568(const          A&&);
eight sink_4_3568(const volatile A&&);

int test4_3568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3568(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3568(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_3568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_3568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_3568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_4_3578(volatile       A&);
five  sink_4_3578(               A&&);
seven sink_4_3578(volatile       A&&);
eight sink_4_3578(const volatile A&&);

int test4_3578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3578(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3578(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_3578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_3578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_3578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_4_3678(volatile       A&);
six   sink_4_3678(const          A&&);
seven sink_4_3678(volatile       A&&);
eight sink_4_3678(const volatile A&&);

int test4_3678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_3678(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_4_3678(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_4_3678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_3678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_3678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_4_4567(const volatile A&);
five  sink_4_4567(               A&&);
six   sink_4_4567(const          A&&);
seven sink_4_4567(volatile       A&&);

int test4_4567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_4567(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_4_4567(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_4567(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_4567(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_4567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_4567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_4567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

four  sink_4_4568(const volatile A&);
five  sink_4_4568(               A&&);
six   sink_4_4568(const          A&&);
eight sink_4_4568(const volatile A&&);

int test4_4568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_4568(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_4_4568(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_4568(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_4568(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_4568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_4568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_4568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_4_4568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_4_4578(const volatile A&);
five  sink_4_4578(               A&&);
seven sink_4_4578(volatile       A&&);
eight sink_4_4578(const volatile A&&);

int test4_4578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_4578(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_4_4578(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_4578(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_4578(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_4578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_4578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_4_4578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_4578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_4_4678(const volatile A&);
six   sink_4_4678(const          A&&);
seven sink_4_4678(volatile       A&&);
eight sink_4_4678(const volatile A&&);

int test4_4678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_4678(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_4_4678(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_4_4678(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_4_4678(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_4_4678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_4678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_4678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

five  sink_4_5678(               A&&);
six   sink_4_5678(const          A&&);
seven sink_4_5678(volatile       A&&);
eight sink_4_5678(const volatile A&&);

int test4_5678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_4_5678(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_4_5678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_4_5678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_4_5678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test4_1234() + test4_1235() + test4_1236() + test4_1237() + test4_1238() +
           test4_1245() + test4_1246() + test4_1247() + test4_1248() + test4_1256() +
           test4_1257() + test4_1258() + test4_1267() + test4_1268() + test4_1278() +
           test4_1345() + test4_1346() + test4_1347() + test4_1348() + test4_1356() +
           test4_1357() + test4_1358() + test4_1367() + test4_1368() + test4_1378() +
           test4_1456() + test4_1457() + test4_1458() + test4_1467() + test4_1468() +
           test4_1478() + test4_1567() + test4_1568() + test4_1578() + test4_1678() +
           test4_2345() + test4_2346() + test4_2347() + test4_2348() + test4_2356() +
           test4_2357() + test4_2358() + test4_2367() + test4_2368() + test4_2378() +
           test4_2456() + test4_2457() + test4_2458() + test4_2467() + test4_2468() +
           test4_2478() + test4_2567() + test4_2568() + test4_2578() + test4_2678() +
           test4_3456() + test4_3457() + test4_3458() + test4_3467() + test4_3468() +
           test4_3478() + test4_3567() + test4_3568() + test4_3578() + test4_3678() +
           test4_4567() + test4_4568() + test4_4578() + test4_4678() + test4_5678();
}
