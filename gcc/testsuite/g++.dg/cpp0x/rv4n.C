// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile }
// { dg-options "-std=c++11" }

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

// 4 at a time

one   sink_4_1234(               A&);  // { dg-message "one sink_4_1234|no known conversion" }
two   sink_4_1234(const          A&);  // { dg-message "note" }
three sink_4_1234(volatile       A&);  // { dg-message "note" }
four  sink_4_1234(const volatile A&);  // { dg-message "note" }

int test4_1234()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1234(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 44 }
    sink_4_1234(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 46 }
    return 0;
}

one   sink_4_1235(               A&);  // { dg-message "one sink_4_1235|no known conversion" }
two   sink_4_1235(const          A&);  // { dg-message "note" }
three sink_4_1235(volatile       A&);  // { dg-message "note" }
five  sink_4_1235(               A&&);  // { dg-message "note" }

int test4_1235()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1235(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 62 }
    sink_4_1235(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 64 }
    sink_4_1235(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 66 }
    return 0;
}

one   sink_4_1236(               A&);  // { dg-message "one sink_4_1236|no known conversion" }
two   sink_4_1236(const          A&);  // { dg-message "note" }
three sink_4_1236(volatile       A&);  // { dg-message "note" }
six   sink_4_1236(const          A&&);  // { dg-message "note" }

int test4_1236()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1236(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 82 }
    sink_4_1236(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 84 }
    sink_4_1236(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 86 }
    return 0;
}

one   sink_4_1237(               A&);  // { dg-message "one sink_4_1237|no known conversion" }
two   sink_4_1237(const          A&);  // { dg-message "note" }
three sink_4_1237(volatile       A&);  // { dg-message "note" }
seven sink_4_1237(volatile       A&&);  // { dg-message "note" }

int test4_1237()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1237(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 102 }
    sink_4_1237(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 104 }
    return 0;
}

one   sink_4_1238(               A&);
two   sink_4_1238(const          A&);
three sink_4_1238(volatile       A&);
eight sink_4_1238(const volatile A&&); // { dg-message "" }

int test4_1238()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1238(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1245(               A&);  // { dg-message "one sink_4_1245|no known conversion" }
two   sink_4_1245(const          A&);  // { dg-message "note" }
four  sink_4_1245(const volatile A&);  // { dg-message "note" }
five  sink_4_1245(               A&&);  // { dg-message "note" }

int test4_1245()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1245(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 135 }
    sink_4_1245(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 137 }
    return 0;
}

one   sink_4_1246(               A&);  // { dg-message "one sink_4_1246|no known conversion" }
two   sink_4_1246(const          A&);  // { dg-message "note" }
four  sink_4_1246(const volatile A&);  // { dg-message "note" }
six   sink_4_1246(const          A&&);  // { dg-message "note" }

int test4_1246()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1246(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 153 }
    sink_4_1246(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 155 }
    return 0;
}

one   sink_4_1247(               A&);  // { dg-message "one sink_4_1247|no known conversion" }
two   sink_4_1247(const          A&);  // { dg-message "note" }
four  sink_4_1247(const volatile A&);  // { dg-message "note" }
seven sink_4_1247(volatile       A&&);  // { dg-message "note" }

int test4_1247()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1247(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 171 }
    return 0;
}

one   sink_4_1256(               A&);  // { dg-message "one sink_4_1256|no known conversion" }
two   sink_4_1256(const          A&);  // { dg-message "note" }
five  sink_4_1256(               A&&);  // { dg-message "note" }
six   sink_4_1256(const          A&&);  // { dg-message "note" }

int test4_1256()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1256(va);           // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 187 }
    sink_4_1256(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 189 }
    sink_4_1256(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 191 }
    sink_4_1256(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 193 }
    return 0;
}

one   sink_4_1257(               A&);  // { dg-message "one sink_4_1257|no known conversion" }
two   sink_4_1257(const          A&);  // { dg-message "note" }
five  sink_4_1257(               A&&);  // { dg-message "note" }
seven sink_4_1257(volatile       A&&);  // { dg-message "" }

int test4_1257()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1257(va);		// { dg-error "lvalue" }
    sink_4_1257(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 210 }
    sink_4_1257(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 212 }
    return 0;
}

one   sink_4_1258(               A&);
two   sink_4_1258(const          A&);
five  sink_4_1258(               A&&);
eight sink_4_1258(const volatile A&&); // { dg-message "" }

int test4_1258()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1258(va);		// { dg-error "lvalue" }
    sink_4_1258(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1267(               A&);  // { dg-message "one sink_4_1267|no known conversion" }
two   sink_4_1267(const          A&);  // { dg-message "note" }
six   sink_4_1267(const          A&&);  // { dg-message "note" }
seven sink_4_1267(volatile       A&&);  // { dg-message "" }

int test4_1267()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1267(va);		// { dg-error "lvalue" }
    sink_4_1267(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 245 }
    sink_4_1267(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 247 }
    sink_4_1267(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 249 }
    return 0;
}

one   sink_4_1268(               A&);
two   sink_4_1268(const          A&);
six   sink_4_1268(const          A&&);
eight sink_4_1268(const volatile A&&); // { dg-message "" }

int test4_1268()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1268(va);		// { dg-error "lvalue" }
    sink_4_1268(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1278(               A&);
two   sink_4_1278(const          A&);
seven sink_4_1278(volatile       A&&); // { dg-message "" }
eight sink_4_1278(const volatile A&&); // { dg-message "" }

int test4_1278()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1278(va);		// { dg-error "lvalue" }
    sink_4_1278(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1345(               A&);  // { dg-message "one sink_4_1345|no known conversion" }
three sink_4_1345(volatile       A&);  // { dg-message "note" }
four  sink_4_1345(const volatile A&);  // { dg-message "note" }
five  sink_4_1345(               A&&);  // { dg-message "note" }

int test4_1345()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1345(c_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 297 }
    sink_4_1345(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 299 }
    sink_4_1345(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 301 }
    return 0;
}

one   sink_4_1346(               A&);  // { dg-message "one sink_4_1346|no known conversion" }
three sink_4_1346(volatile       A&);  // { dg-message "note" }
four  sink_4_1346(const volatile A&);  // { dg-message "note" }
six   sink_4_1346(const          A&&);  // { dg-message "note" }

int test4_1346()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1346(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 317 }
    sink_4_1346(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 319 }
    return 0;
}

one   sink_4_1347(               A&);  // { dg-message "one sink_4_1347|no known conversion" }
three sink_4_1347(volatile       A&);  // { dg-message "note" }
four  sink_4_1347(const volatile A&);  // { dg-message "note" }
seven sink_4_1347(volatile       A&&);  // { dg-message "note" }

int test4_1347()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1347(c_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 335 }
    sink_4_1347(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 337 }
    return 0;
}

one   sink_4_1356(               A&);  // { dg-message "one sink_4_1356|no known conversion" }
three sink_4_1356(volatile       A&);  // { dg-message "note" }
five  sink_4_1356(               A&&);  // { dg-message "note" }
six   sink_4_1356(const          A&&);  // { dg-message "" }

int test4_1356()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1356(ca);		// { dg-error "lvalue" }
    sink_4_1356(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 354 }
    sink_4_1356(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 356 }
    sink_4_1356(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 358 }
    return 0;
}

one   sink_4_1357(               A&);  // { dg-message "one sink_4_1357|no known conversion" }
three sink_4_1357(volatile       A&);  // { dg-message "note" }
five  sink_4_1357(               A&&);  // { dg-message "note" }
seven sink_4_1357(volatile       A&&);  // { dg-message "note" }

int test4_1357()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1357(ca);           // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 374 }
    sink_4_1357(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 376 }
    sink_4_1357(c_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 378 }
    sink_4_1357(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 380 }
    return 0;
}

one   sink_4_1358(               A&);
three sink_4_1358(volatile       A&);
five  sink_4_1358(               A&&);
eight sink_4_1358(const volatile A&&); // { dg-message "" }

int test4_1358()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1358(ca);		// { dg-error "lvalue" }
    sink_4_1358(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1367(               A&);  // { dg-message "one sink_4_1367|no known conversion" }
three sink_4_1367(volatile       A&);  // { dg-message "note" }
six   sink_4_1367(const          A&&);  // { dg-message "" }
seven sink_4_1367(volatile       A&&);  // { dg-message "note" }

int test4_1367()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1367(ca);		// { dg-error "lvalue" }
    sink_4_1367(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 413 }
    sink_4_1367(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 415 }
    sink_4_1367(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 417 }
    return 0;
}

one   sink_4_1368(               A&);
three sink_4_1368(volatile       A&);
six   sink_4_1368(const          A&&); // { dg-message "" }
eight sink_4_1368(const volatile A&&); // { dg-message "" }

int test4_1368()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1368(ca);		// { dg-error "lvalue" }
    sink_4_1368(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1378(               A&);
three sink_4_1378(volatile       A&);
seven sink_4_1378(volatile       A&&);
eight sink_4_1378(const volatile A&&); // { dg-message "" }

int test4_1378()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1378(ca);		// { dg-error "lvalue" }
    sink_4_1378(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1456(               A&);  // { dg-message "one sink_4_1456|no known conversion" }
four  sink_4_1456(const volatile A&);  // { dg-message "note" }
five  sink_4_1456(               A&&);  // { dg-message "note" }
six   sink_4_1456(const          A&&);  // { dg-message "note" }

int test4_1456()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1456(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 465 }
    sink_4_1456(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 467 }
    return 0;
}

one   sink_4_1457(               A&);  // { dg-message "one sink_4_1457|no known conversion" }
four  sink_4_1457(const volatile A&);  // { dg-message "note" }
five  sink_4_1457(               A&&);  // { dg-message "note" }
seven sink_4_1457(volatile       A&&);  // { dg-message "note" }

int test4_1457()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1457(c_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 483 }
    sink_4_1457(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 485 }
    return 0;
}

one   sink_4_1467(               A&);  // { dg-message "one sink_4_1467|no known conversion" }
four  sink_4_1467(const volatile A&);  // { dg-message "note" }
six   sink_4_1467(const          A&&);  // { dg-message "note" }
seven sink_4_1467(volatile       A&&);  // { dg-message "note" }

int test4_1467()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1467(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 501 }
    sink_4_1467(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 503 }
    return 0;
}

one   sink_4_1567(               A&);  // { dg-message "one sink_4_1567|no known conversion" }
five  sink_4_1567(               A&&);  // { dg-message "note" }
six   sink_4_1567(const          A&&);  // { dg-message "" }
seven sink_4_1567(volatile       A&&);  // { dg-message "" }

int test4_1567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1567(ca);	       // { dg-error "lvalue" }
    sink_4_1567(va);	       // { dg-error "lvalue" }
    sink_4_1567(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 521 }
    sink_4_1567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 523 }
    return 0;
}

one   sink_4_1568(               A&);
five  sink_4_1568(               A&&);
six   sink_4_1568(const          A&&); // { dg-message "" }
eight sink_4_1568(const volatile A&&); // { dg-message "" }

int test4_1568()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1568(ca);		// { dg-error "lvalue" }
    sink_4_1568(va);		// { dg-error "lvalue" }
    sink_4_1568(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1578(               A&);
five  sink_4_1578(               A&&);
seven sink_4_1578(volatile       A&&); // { dg-message "" }
eight sink_4_1578(const volatile A&&); // { dg-message "" }

int test4_1578()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1578(ca);		// { dg-error "lvalue" }
    sink_4_1578(va);		// { dg-error "lvalue" }
    sink_4_1578(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_4_1678(               A&);
six   sink_4_1678(const          A&&);  // { dg-message "" }
seven sink_4_1678(volatile       A&&);  // { dg-message "" }
eight sink_4_1678(const volatile A&&);  // { dg-message "" }

int test4_1678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_1678(ca);		// { dg-error "lvalue" }
    sink_4_1678(va);		// { dg-error "lvalue" }
    sink_4_1678(cva);		// { dg-error "lvalue" }
    sink_4_1678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 576 }
    return 0;
}

two   sink_4_2345(const          A&);  // { dg-message "two sink_4_2345|no known conversion" }
three sink_4_2345(volatile       A&);  // { dg-message "note" }
four  sink_4_2345(const volatile A&);  // { dg-message "note" }
five  sink_4_2345(               A&&);  // { dg-message "note" }

int test4_2345()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2345(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 592 }
    sink_4_2345(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 594 }
    sink_4_2345(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 596 }
    return 0;
}

two   sink_4_2346(const          A&);  // { dg-message "two sink_4_2346|no known conversion" }
three sink_4_2346(volatile       A&);  // { dg-message "note" }
four  sink_4_2346(const volatile A&);  // { dg-message "note" }
six   sink_4_2346(const          A&&);  // { dg-message "note" }

int test4_2346()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2346(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 612 }
    sink_4_2346(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 614 }
    sink_4_2346(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 616 }
    return 0;
}

two   sink_4_2347(const          A&);  // { dg-message "two sink_4_2347|no known conversion" }
three sink_4_2347(volatile       A&);  // { dg-message "note" }
four  sink_4_2347(const volatile A&);  // { dg-message "note" }
seven sink_4_2347(volatile       A&&);  // { dg-message "note" }

int test4_2347()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2347(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 632 }
    sink_4_2347(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 634 }
    return 0;
}

two   sink_4_2348(const          A&);  // { dg-message "note" }
three sink_4_2348(volatile       A&);  // { dg-message "note" }
four  sink_4_2348(const volatile A&);  // { dg-message "note" }
eight sink_4_2348(const volatile A&&);  // { dg-message "note" }

int test4_2348()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2348(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 650 }
    return 0;
}

two   sink_4_2356(const          A&);  // { dg-message "two sink_4_2356|no known conversion" }
three sink_4_2356(volatile       A&);  // { dg-message "note" }
five  sink_4_2356(               A&&);  // { dg-message "note" }
six   sink_4_2356(const          A&&);  // { dg-message "note" }

int test4_2356()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2356(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 666 }
    sink_4_2356(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 668 }
    sink_4_2356(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 670 }
    sink_4_2356(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 672 }
    return 0;
}

two   sink_4_2357(const          A&);  // { dg-message "two sink_4_2357|no known conversion" }
three sink_4_2357(volatile       A&);  // { dg-message "note" }
five  sink_4_2357(               A&&);  // { dg-message "note" }
seven sink_4_2357(volatile       A&&);  // { dg-message "note" }

int test4_2357()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2357(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 688 }
    sink_4_2357(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 690 }
    sink_4_2357(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 692 }
    return 0;
}

two   sink_4_2358(const          A&);  // { dg-message "note" }
three sink_4_2358(volatile       A&);  // { dg-message "note" }
five  sink_4_2358(               A&&);  // { dg-message "note" }
eight sink_4_2358(const volatile A&&);  // { dg-message "" }

int test4_2358()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2358(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 708 }
    sink_4_2358(cva); // { dg-error "lvalue" }
    return 0;
}

two   sink_4_2367(const          A&);  // { dg-message "two sink_4_2367|no known conversion" }
three sink_4_2367(volatile       A&);  // { dg-message "note" }
six   sink_4_2367(const          A&&);  // { dg-message "note" }
seven sink_4_2367(volatile       A&&);  // { dg-message "note" }

int test4_2367()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2367(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 725 }
    sink_4_2367(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 727 }
    sink_4_2367(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 729 }
    sink_4_2367(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 731 }
    return 0;
}

two   sink_4_2368(const          A&);  // { dg-message "note" }
three sink_4_2368(volatile       A&);  // { dg-message "note" }
six   sink_4_2368(const          A&&);  // { dg-message "note" }
eight sink_4_2368(const volatile A&&);  // { dg-message "" }

int test4_2368()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2368(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 747 }
    sink_4_2368(cva); // { dg-error "lvalue" }
    return 0;
}

two   sink_4_2378(const          A&);  // { dg-message "note" }
three sink_4_2378(volatile       A&);  // { dg-message "note" }
seven sink_4_2378(volatile       A&&);  // { dg-message "note" }
eight sink_4_2378(const volatile A&&);  // { dg-message "" }

int test4_2378()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2378(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 764 }
    sink_4_2378(cva); // { dg-error "lvalue" }
    return 0;
}

two   sink_4_2456(const          A&);  // { dg-message "two sink_4_2456|no known conversion" }
four  sink_4_2456(const volatile A&);  // { dg-message "note" }
five  sink_4_2456(               A&&);  // { dg-message "note" }
six   sink_4_2456(const          A&&);  // { dg-message "note" }

int test4_2456()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2456(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 781 }
    sink_4_2456(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 783 }
    return 0;
}

two   sink_4_2457(const          A&);  // { dg-message "two sink_4_2457|no known conversion" }
four  sink_4_2457(const volatile A&);  // { dg-message "note" }
five  sink_4_2457(               A&&);  // { dg-message "note" }
seven sink_4_2457(volatile       A&&);  // { dg-message "note" }

int test4_2457()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2457(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 799 }
    return 0;
}

two   sink_4_2467(const          A&);  // { dg-message "two sink_4_2467|no known conversion" }
four  sink_4_2467(const volatile A&);  // { dg-message "note" }
six   sink_4_2467(const          A&&);  // { dg-message "note" }
seven sink_4_2467(volatile       A&&);  // { dg-message "note" }

int test4_2467()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2467(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 815 }
    sink_4_2467(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 817 }
    return 0;
}

two   sink_4_2567(const          A&);  // { dg-message "two sink_4_2567|no known conversion" }
five  sink_4_2567(               A&&);  // { dg-message "note" }
six   sink_4_2567(const          A&&);  // { dg-message "note" }
seven sink_4_2567(volatile       A&&);  // { dg-message "" }

int test4_2567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2567(va);		// { dg-error "lvalue" }
    sink_4_2567(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 834 }
    sink_4_2567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 836 }
    return 0;
}

two   sink_4_2568(const          A&);
five  sink_4_2568(               A&&);
six   sink_4_2568(const          A&&);
eight sink_4_2568(const volatile A&&); // { dg-message "" }

int test4_2568()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2568(va); // { dg-error "lvalue" }
    sink_4_2568(cva); // { dg-error "lvalue" }
    return 0;
}

two   sink_4_2578(const          A&);
five  sink_4_2578(               A&&);
seven sink_4_2578(volatile       A&&); // { dg-message "" }
eight sink_4_2578(const volatile A&&); // { dg-message "" }

int test4_2578()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2578(va); // { dg-error "lvalue" }
    sink_4_2578(cva); // { dg-error "lvalue" }
    return 0;
}

two   sink_4_2678(const          A&);  // { dg-message "note" }
six   sink_4_2678(const          A&&);  // { dg-message "note" }
seven sink_4_2678(volatile       A&&);  // { dg-message "" }
eight sink_4_2678(const volatile A&&);  // { dg-message "" }

int test4_2678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_2678(va); // { dg-error "lvalue" }
    sink_4_2678(cva); // { dg-error "lvalue" }
    sink_4_2678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 886 }
    return 0;
}

three sink_4_3456(volatile       A&);  // { dg-message "three sink_4_3456|no known conversion" }
four  sink_4_3456(const volatile A&);  // { dg-message "note" }
five  sink_4_3456(               A&&);  // { dg-message "note" }
six   sink_4_3456(const          A&&);  // { dg-message "note" }

int test4_3456()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3456(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 902 }
    sink_4_3456(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 904 }
    return 0;
}

three sink_4_3457(volatile       A&);  // { dg-message "three sink_4_3457|no known conversion" }
four  sink_4_3457(const volatile A&);  // { dg-message "note" }
five  sink_4_3457(               A&&);  // { dg-message "note" }
seven sink_4_3457(volatile       A&&);  // { dg-message "note" }

int test4_3457()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3457(c_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 920 }
    sink_4_3457(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 922 }
    return 0;
}

three sink_4_3467(volatile       A&);  // { dg-message "three sink_4_3467|no known conversion" }
four  sink_4_3467(const volatile A&);  // { dg-message "note" }
six   sink_4_3467(const          A&&);  // { dg-message "note" }
seven sink_4_3467(volatile       A&&);  // { dg-message "note" }

int test4_3467()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3467(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 938 }
    sink_4_3467(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 940 }
    return 0;
}

three sink_4_3567(volatile       A&);  // { dg-message "three sink_4_3567|no known conversion" }
five  sink_4_3567(               A&&);  // { dg-message "note" }
six   sink_4_3567(const          A&&);  // { dg-message "" }
seven sink_4_3567(volatile       A&&);  // { dg-message "note" }

int test4_3567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3567(ca);		// { dg-error "lvalue" }
    sink_4_3567(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 957 }
    sink_4_3567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 959 }
    return 0;
}

three sink_4_3568(volatile       A&);
five  sink_4_3568(               A&&);
six   sink_4_3568(const          A&&); // { dg-message "" }
eight sink_4_3568(const volatile A&&); // { dg-message "" }

int test4_3568()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3568(ca); // { dg-error "lvalue" }
    sink_4_3568(cva); // { dg-error "lvalue" }
    return 0;
}

three sink_4_3578(volatile       A&);
five  sink_4_3578(               A&&);
seven sink_4_3578(volatile       A&&);
eight sink_4_3578(const volatile A&&); // { dg-message "" }

int test4_3578()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3578(ca); // { dg-error "lvalue" }
    sink_4_3578(cva); // { dg-error "lvalue" }
    return 0;
}

three sink_4_3678(volatile       A&);
six   sink_4_3678(const          A&&);  // { dg-message "" }
seven sink_4_3678(volatile       A&&);  // { dg-message "note" }
eight sink_4_3678(const volatile A&&);  // { dg-message "" }

int test4_3678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_3678(ca);		// { dg-error "lvalue" }
    sink_4_3678(cva);		// { dg-error "lvalue" }
    sink_4_3678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 1009 }
    return 0;
}

four  sink_4_4567(const volatile A&);  // { dg-message "note" }
five  sink_4_4567(               A&&);  // { dg-message "note" }
six   sink_4_4567(const          A&&);  // { dg-message "note" }
seven sink_4_4567(volatile       A&&);  // { dg-message "note" }

int test4_4567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_4567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 1025 }
    return 0;
}

four  sink_4_4678(const volatile A&);
six   sink_4_4678(const          A&&);  // { dg-message "note" }
seven sink_4_4678(volatile       A&&);  // { dg-message "note" }
eight sink_4_4678(const volatile A&&);  // { dg-message "note" }

int test4_4678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_4678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 1041 }
    return 0;
}

five  sink_4_5678(               A&&); // { dg-message "" }
six   sink_4_5678(const          A&&); // { dg-message "" }
seven sink_4_5678(volatile       A&&); // { dg-message "" }
eight sink_4_5678(const volatile A&&); // { dg-message "" }

int test4_5678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_4_5678(a);		// { dg-error "lvalue" }
    sink_4_5678(ca);		// { dg-error "lvalue" }
    sink_4_5678(va);		// { dg-error "lvalue" }
    sink_4_5678(cva);		// { dg-error "lvalue" }
    return 0;
}

int main()
{
    return test4_1235() + test4_1236() + test4_1237() + test4_1256() + test4_1257() +
           test4_1267() + test4_1356() + test4_1357() + test4_1467() + test4_1567() +
           test4_1678() + test4_2345() + test4_2346() + test4_2347() + test4_2348() +
           test4_2356() + test4_2357() + test4_2358() + test4_2367() + test4_2368() +
           test4_2378() + test4_2467() + test4_2567() + test4_2678() + test4_3467() +
           test4_3567() + test4_3678() + test4_4678();
}
