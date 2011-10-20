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

// 6 at a time

one   sink_6_123456(               A&);  // { dg-message "one sink_6_123456|no known conversion" }
two   sink_6_123456(const          A&);  // { dg-message "note" }
three sink_6_123456(volatile       A&);  // { dg-message "note" }
four  sink_6_123456(const volatile A&);  // { dg-message "note" }
five  sink_6_123456(               A&&);  // { dg-message "note" }
six   sink_6_123456(const          A&&);  // { dg-message "note" }

int test6_123456()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123456(v_source());   // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 46 }
    sink_6_123456(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 48 }
    return 0;
}

one   sink_6_123457(               A&);  // { dg-message "one sink_6_123457|no known conversion" }
two   sink_6_123457(const          A&);  // { dg-message "note" }
three sink_6_123457(volatile       A&);  // { dg-message "note" }
four  sink_6_123457(const volatile A&);  // { dg-message "note" }
five  sink_6_123457(               A&&);  // { dg-message "note" }
seven sink_6_123457(volatile       A&&);  // { dg-message "note" }

int test6_123457()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123457(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 66 }
    return 0;
}

two   sink_6_235678(const          A&);  // { dg-message "note" }
three sink_6_235678(volatile       A&);  // { dg-message "note" }
five  sink_6_235678(               A&&);  // { dg-message "note" }
six   sink_6_235678(const          A&&);  // { dg-message "note" }
seven sink_6_235678(volatile       A&&);  // { dg-message "note" }
eight sink_6_235678(const volatile A&&);  // { dg-message "" }

int test6_235678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_235678(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 84 }
    sink_6_235678(cva);		// { dg-error "lvalue" }
    return 0;
}

two   sink_6_234678(const          A&);  // { dg-message "note" }
three sink_6_234678(volatile       A&);  // { dg-message "note" }
four  sink_6_234678(const volatile A&);  // { dg-message "note" }
six   sink_6_234678(const          A&&);  // { dg-message "note" }
seven sink_6_234678(volatile       A&&);  // { dg-message "note" }
eight sink_6_234678(const volatile A&&);  // { dg-message "note" }

int test6_234678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_234678(a);         // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 103 }
    sink_6_234678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 105 }
    return 0;
}

two   sink_6_234578(const          A&);  // { dg-message "note" }
three sink_6_234578(volatile       A&);  // { dg-message "note" }
four  sink_6_234578(const volatile A&);  // { dg-message "note" }
five  sink_6_234578(               A&&);  // { dg-message "note" }
seven sink_6_234578(volatile       A&&);  // { dg-message "note" }
eight sink_6_234578(const volatile A&&);  // { dg-message "note" }

int test6_234578()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_234578(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 123 }
    return 0;
}

two   sink_6_234568(const          A&);  // { dg-message "note" }
three sink_6_234568(volatile       A&);  // { dg-message "note" }
four  sink_6_234568(const volatile A&);  // { dg-message "note" }
five  sink_6_234568(               A&&);  // { dg-message "note" }
six   sink_6_234568(const          A&&);  // { dg-message "note" }
eight sink_6_234568(const volatile A&&);  // { dg-message "note" }

int test6_234568()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_234568(a);  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 141 }
    return 0;
}

two   sink_6_234567(const          A&);  // { dg-message "two sink_6_234567|no known conversion" }
three sink_6_234567(volatile       A&);  // { dg-message "note" }
four  sink_6_234567(const volatile A&);  // { dg-message "note" }
five  sink_6_234567(               A&&);  // { dg-message "note" }
six   sink_6_234567(const          A&&);  // { dg-message "note" }
seven sink_6_234567(volatile       A&&);  // { dg-message "note" }

int test6_234567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_234567(a);            // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 159 }
    sink_6_234567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 161 }
    return 0;
}

one   sink_6_134678(               A&);
three sink_6_134678(volatile       A&);
four  sink_6_134678(const volatile A&);
six   sink_6_134678(const          A&&);  // { dg-message "note" }
seven sink_6_134678(volatile       A&&);  // { dg-message "note" }
eight sink_6_134678(const volatile A&&);  // { dg-message "note" }

int test6_134678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_134678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 179 }
    return 0;
}

one   sink_6_124678(               A&);
two   sink_6_124678(const          A&);  // { dg-message "note" }
four  sink_6_124678(const volatile A&);
six   sink_6_124678(const          A&&);  // { dg-message "note" }
seven sink_6_124678(volatile       A&&);  // { dg-message "note" }
eight sink_6_124678(const volatile A&&);  // { dg-message "note" }

int test6_124678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_124678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 197 }
    return 0;
}

one   sink_6_123678(               A&);
two   sink_6_123678(const          A&);  // { dg-message "note" }
three sink_6_123678(volatile       A&);
six   sink_6_123678(const          A&&);  // { dg-message "note" }
seven sink_6_123678(volatile       A&&);  // { dg-message "note" }
eight sink_6_123678(const volatile A&&);  // { dg-message "" }

int test6_123678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123678(cva);		// { dg-error "lvalue" }
    sink_6_123678(source());  // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 216 }
    return 0;
}

one   sink_6_123567(               A&);  // { dg-message "one sink_6_123567|no known conversion" }
two   sink_6_123567(const          A&);  // { dg-message "note" }
three sink_6_123567(volatile       A&);  // { dg-message "note" }
five  sink_6_123567(               A&&);  // { dg-message "note" }
six   sink_6_123567(const          A&&);  // { dg-message "note" }
seven sink_6_123567(volatile       A&&);  // { dg-message "note" }

int test6_123567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123567(cva);          // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 234 }
    sink_6_123567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 236 }
    return 0;
}

one   sink_6_123568(               A&);
two   sink_6_123568(const          A&);
three sink_6_123568(volatile       A&);
five  sink_6_123568(               A&&);
six   sink_6_123568(const          A&&);
eight sink_6_123568(const volatile A&&); // { dg-message "" }

int test6_123568()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123568(cva); // { dg-error "lvalue" }
    return 0;
}

one   sink_6_123578(               A&);
two   sink_6_123578(const          A&);
three sink_6_123578(volatile       A&);
five  sink_6_123578(               A&&);
seven sink_6_123578(volatile       A&&);
eight sink_6_123578(const volatile A&&); // { dg-message "" }

int test6_123578()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123578(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_6_123467(               A&);  // { dg-message "one sink_6_123467|no known conversion" }
two   sink_6_123467(const          A&);  // { dg-message "note" }
three sink_6_123467(volatile       A&);  // { dg-message "note" }
four  sink_6_123467(const volatile A&);  // { dg-message "note" }
six   sink_6_123467(const          A&&);  // { dg-message "note" }
seven sink_6_123467(volatile       A&&);  // { dg-message "note" }

int test6_123467()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_123467(source());     // { dg-error "ambiguous" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 288 }
    sink_6_123467(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 290 }
    return 0;
}

one   sink_6_124567(               A&);  // { dg-message "one sink_6_124567|no known conversion" }
two   sink_6_124567(const          A&);  // { dg-message "note" }
four  sink_6_124567(const volatile A&);  // { dg-message "note" }
five  sink_6_124567(               A&&);  // { dg-message "note" }
six   sink_6_124567(const          A&&);  // { dg-message "note" }
seven sink_6_124567(volatile       A&&);  // { dg-message "note" }

int test6_124567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_124567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 308 }
    return 0;
}

one   sink_6_125678(               A&);
two   sink_6_125678(const          A&);
five  sink_6_125678(               A&&);
six   sink_6_125678(const          A&&);
seven sink_6_125678(volatile       A&&); // { dg-message "" }
eight sink_6_125678(const volatile A&&); // { dg-message "" }

int test6_125678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_125678(va);		// { dg-error "lvalue" }
    sink_6_125678(cva);		// { dg-error "lvalue" }
    return 0;
}

one   sink_6_134567(               A&);  // { dg-message "one sink_6_134567|no known conversion" }
three sink_6_134567(volatile       A&);  // { dg-message "note" }
four  sink_6_134567(const volatile A&);  // { dg-message "note" }
five  sink_6_134567(               A&&);  // { dg-message "note" }
six   sink_6_134567(const          A&&);  // { dg-message "note" }
seven sink_6_134567(volatile       A&&);  // { dg-message "note" }

int test6_134567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_134567(cv_source());  // { dg-error "no match" }
    // { dg-message "candidate" "candidate note" { target *-*-* } 344 }
    return 0;
}

one   sink_6_135678(               A&);
three sink_6_135678(volatile       A&);
five  sink_6_135678(               A&&);
six   sink_6_135678(const          A&&); // { dg-message "" }
seven sink_6_135678(volatile       A&&);
eight sink_6_135678(const volatile A&&); // { dg-message "" }

int test6_135678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_6_135678(ca);		// { dg-error "lvalue" }
    sink_6_135678(cva);		// { dg-error "lvalue" }
    return 0;
}

int main()
{
    return test6_235678() + test6_234678() + test6_234578() + test6_234568() +
           test6_234567() + test6_134678() + test6_124678() + test6_123678() +
           test6_123567();
}
