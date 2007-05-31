// I, Howard Hinnant, hereby place this code in the public domain.

// Test overlaod resolution among referece types

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

// 5 at a time

one   sink_5_12345(               A&);  // { dg-error "" }
two   sink_5_12345(const          A&);  // { dg-error "" }
three sink_5_12345(volatile       A&);  // { dg-error "" }
four  sink_5_12345(const volatile A&);  // { dg-error "" }
five  sink_5_12345(               A&&);  // { dg-error "" }

int test5_12345()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12345(v_source());   // { dg-error "no match" }
    sink_5_12345(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12346(               A&);  // { dg-error "" }
two   sink_5_12346(const          A&);  // { dg-error "" }
three sink_5_12346(volatile       A&);  // { dg-error "" }
four  sink_5_12346(const volatile A&);  // { dg-error "" }
six   sink_5_12346(const          A&&);  // { dg-error "" }

int test5_12346()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12346(v_source());   // { dg-error "no match" }
    sink_5_12346(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12347(               A&);  // { dg-error "" }
two   sink_5_12347(const          A&);  // { dg-error "" }
three sink_5_12347(volatile       A&);  // { dg-error "" }
four  sink_5_12347(const volatile A&);  // { dg-error "" }
seven sink_5_12347(volatile       A&&);  // { dg-error "" }

int test5_12347()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12347(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12356(               A&);  // { dg-error "" }
two   sink_5_12356(const          A&);  // { dg-error "" }
three sink_5_12356(volatile       A&);  // { dg-error "" }
five  sink_5_12356(               A&&);  // { dg-error "" }
six   sink_5_12356(const          A&&);  // { dg-error "" }

int test5_12356()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12356(cva);          // { dg-error "no match" }
    sink_5_12356(v_source());   // { dg-error "no match" }
    sink_5_12356(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12357(               A&);  // { dg-error "" }
two   sink_5_12357(const          A&);  // { dg-error "" }
three sink_5_12357(volatile       A&);  // { dg-error "" }
five  sink_5_12357(               A&&);  // { dg-error "" }
seven sink_5_12357(volatile       A&&);  // { dg-error "" }

int test5_12357()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12357(cva);          // { dg-error "no match" }
    sink_5_12357(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12367(               A&);  // { dg-error "" }
two   sink_5_12367(const          A&);  // { dg-error "" }
three sink_5_12367(volatile       A&);  // { dg-error "" }
six   sink_5_12367(const          A&&);  // { dg-error "" }
seven sink_5_12367(volatile       A&&);  // { dg-error "" }

int test5_12367()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12367(cva);          // { dg-error "no match" }
    sink_5_12367(source());     // { dg-error "ambiguous" }
    sink_5_12367(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12456(               A&);  // { dg-error "" }
two   sink_5_12456(const          A&);  // { dg-error "" }
four  sink_5_12456(const volatile A&);  // { dg-error "" }
five  sink_5_12456(               A&&);  // { dg-error "" }
six   sink_5_12456(const          A&&);  // { dg-error "" }

int test5_12456()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12456(v_source());   // { dg-error "no match" }
    sink_5_12456(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12457(               A&);  // { dg-error "" }
two   sink_5_12457(const          A&);  // { dg-error "" }
four  sink_5_12457(const volatile A&);  // { dg-error "" }
five  sink_5_12457(               A&&);  // { dg-error "" }
seven sink_5_12457(volatile       A&&);  // { dg-error "" }

int test5_12457()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12457(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12467(               A&);  // { dg-error "" }
two   sink_5_12467(const          A&);  // { dg-error "" }
four  sink_5_12467(const volatile A&);  // { dg-error "" }
six   sink_5_12467(const          A&&);  // { dg-error "" }
seven sink_5_12467(volatile       A&&);  // { dg-error "" }

int test5_12467()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12467(source());     // { dg-error "ambiguous" }
    sink_5_12467(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12567(               A&);  // { dg-error "" }
two   sink_5_12567(const          A&);  // { dg-error "" }
five  sink_5_12567(               A&&);  // { dg-error "" }
six   sink_5_12567(const          A&&);  // { dg-error "" }
seven sink_5_12567(volatile       A&&);  // { dg-error "" }

int test5_12567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12567(cva);          // { dg-error "no match" }
    sink_5_12567(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_12678(               A&);
two   sink_5_12678(const          A&);  // { dg-error "" }
six   sink_5_12678(const          A&&);  // { dg-error "" }
seven sink_5_12678(volatile       A&&);  // { dg-error "" }
eight sink_5_12678(const volatile A&&);  // { dg-error "" }

int test5_12678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_12678(source());  // { dg-error "ambiguous" }
    return 0;
}

one   sink_5_13456(               A&);  // { dg-error "" }
three sink_5_13456(volatile       A&);  // { dg-error "" }
four  sink_5_13456(const volatile A&);  // { dg-error "" }
five  sink_5_13456(               A&&);  // { dg-error "" }
six   sink_5_13456(const          A&&);  // { dg-error "" }

int test5_13456()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_13456(v_source());   // { dg-error "no match" }
    sink_5_13456(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_13457(               A&);  // { dg-error "" }
three sink_5_13457(volatile       A&);  // { dg-error "" }
four  sink_5_13457(const volatile A&);  // { dg-error "" }
five  sink_5_13457(               A&&);  // { dg-error "" }
seven sink_5_13457(volatile       A&&);  // { dg-error "" }

int test5_13457()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_13457(c_source());   // { dg-error "no match" }
    sink_5_13457(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_13467(               A&);  // { dg-error "" }
three sink_5_13467(volatile       A&);  // { dg-error "" }
four  sink_5_13467(const volatile A&);  // { dg-error "" }
six   sink_5_13467(const          A&&);  // { dg-error "" }
seven sink_5_13467(volatile       A&&);  // { dg-error "" }

int test5_13467()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_13467(source());     // { dg-error "ambiguous" }
    sink_5_13467(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_13567(               A&);  // { dg-error "" }
three sink_5_13567(volatile       A&);  // { dg-error "" }
five  sink_5_13567(               A&&);  // { dg-error "" }
six   sink_5_13567(const          A&&);  // { dg-error "" }
seven sink_5_13567(volatile       A&&);  // { dg-error "" }

int test5_13567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_13567(cva);          // { dg-error "no match" }
    sink_5_13567(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_13678(               A&);
three sink_5_13678(volatile       A&);
six   sink_5_13678(const          A&&);  // { dg-error "" }
seven sink_5_13678(volatile       A&&);  // { dg-error "" }
eight sink_5_13678(const volatile A&&);  // { dg-error "" }

int test5_13678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_13678(source());  // { dg-error "ambiguous" }
    return 0;
}

one   sink_5_14567(               A&);  // { dg-error "" }
four  sink_5_14567(const volatile A&);  // { dg-error "" }
five  sink_5_14567(               A&&);  // { dg-error "" }
six   sink_5_14567(const          A&&);  // { dg-error "" }
seven sink_5_14567(volatile       A&&);  // { dg-error "" }

int test5_14567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_14567(cv_source());  // { dg-error "no match" }
    return 0;
}

one   sink_5_14678(               A&);
four  sink_5_14678(const volatile A&);
six   sink_5_14678(const          A&&);  // { dg-error "" }
seven sink_5_14678(volatile       A&&);  // { dg-error "" }
eight sink_5_14678(const volatile A&&);  // { dg-error "" }

int test5_14678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_14678(source());  // { dg-error "ambiguous" }
    return 0;
}

two   sink_5_23456(const          A&);  // { dg-error "" }
three sink_5_23456(volatile       A&);  // { dg-error "" }
four  sink_5_23456(const volatile A&);  // { dg-error "" }
five  sink_5_23456(               A&&);  // { dg-error "" }
six   sink_5_23456(const          A&&);  // { dg-error "" }

int test5_23456()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23456(a);            // { dg-error "ambiguous" }
    sink_5_23456(v_source());   // { dg-error "no match" }
    sink_5_23456(cv_source());  // { dg-error "no match" }
    return 0;
}

two   sink_5_23457(const          A&);  // { dg-error "" }
three sink_5_23457(volatile       A&);  // { dg-error "" }
four  sink_5_23457(const volatile A&);  // { dg-error "" }
five  sink_5_23457(               A&&);  // { dg-error "" }
seven sink_5_23457(volatile       A&&);  // { dg-error "" }

int test5_23457()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23457(a);            // { dg-error "ambiguous" }
    sink_5_23457(cv_source());  // { dg-error "no match" }
    return 0;
}

two   sink_5_23458(const          A&);  // { dg-error "" }
three sink_5_23458(volatile       A&);  // { dg-error "" }
four  sink_5_23458(const volatile A&);  // { dg-error "" }
five  sink_5_23458(               A&&);  // { dg-error "" }
eight sink_5_23458(const volatile A&&);  // { dg-error "" }

int test5_23458()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23458(a);  // { dg-error "ambiguous" }
    return 0;
}

two   sink_5_23467(const          A&);  // { dg-error "" }
three sink_5_23467(volatile       A&);  // { dg-error "" }
four  sink_5_23467(const volatile A&);  // { dg-error "" }
six   sink_5_23467(const          A&&);  // { dg-error "" }
seven sink_5_23467(volatile       A&&);  // { dg-error "" }

int test5_23467()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23467(a);            // { dg-error "ambiguous" }
    sink_5_23467(source());     // { dg-error "ambiguous" }
    sink_5_23467(cv_source());  // { dg-error "no match" }
    return 0;
}

two   sink_5_23468(const          A&);  // { dg-error "" }
three sink_5_23468(volatile       A&);  // { dg-error "" }
four  sink_5_23468(const volatile A&);  // { dg-error "" }
six   sink_5_23468(const          A&&);  // { dg-error "" }
eight sink_5_23468(const volatile A&&);  // { dg-error "" }

int test5_23468()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23468(a);  // { dg-error "ambiguous" }
   return 0;
}

two   sink_5_23478(const          A&);  // { dg-error "" }
three sink_5_23478(volatile       A&);  // { dg-error "" }
four  sink_5_23478(const volatile A&);  // { dg-error "" }
seven sink_5_23478(volatile       A&&);  // { dg-error "" }
eight sink_5_23478(const volatile A&&);  // { dg-error "" }

int test5_23478()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23478(a);  // { dg-error "ambiguous" }
    return 0;
}

two   sink_5_23567(const          A&);  // { dg-error "" }
three sink_5_23567(volatile       A&);  // { dg-error "" }
five  sink_5_23567(               A&&);  // { dg-error "" }
six   sink_5_23567(const          A&&);  // { dg-error "" }
seven sink_5_23567(volatile       A&&);  // { dg-error "" }

int test5_23567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23567(a);            // { dg-error "ambiguous" }
    sink_5_23567(cva);          // { dg-error "no match" }
    sink_5_23567(cv_source());  // { dg-error "no match" }
    return 0;
}

two   sink_5_23568(const          A&);  // { dg-error "" }
three sink_5_23568(volatile       A&);  // { dg-error "" }
five  sink_5_23568(               A&&);  // { dg-error "" }
six   sink_5_23568(const          A&&);  // { dg-error "" }
eight sink_5_23568(const volatile A&&);  // { dg-error "" }

int test5_23568()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23568(a);  // { dg-error "ambiguous" }
    return 0;
}

two   sink_5_23578(const          A&);  // { dg-error "" }
three sink_5_23578(volatile       A&);  // { dg-error "" }
five  sink_5_23578(               A&&);  // { dg-error "" }
seven sink_5_23578(volatile       A&&);  // { dg-error "" }
eight sink_5_23578(const volatile A&&);  // { dg-error "" }

int test5_23578()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23578(a);  // { dg-error "ambiguous" }
    return 0;
}

two   sink_5_23678(const          A&);  // { dg-error "" }
three sink_5_23678(volatile       A&);  // { dg-error "" }
six   sink_5_23678(const          A&&);  // { dg-error "" }
seven sink_5_23678(volatile       A&&);  // { dg-error "" }
eight sink_5_23678(const volatile A&&);  // { dg-error "" }

int test5_23678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_23678(a);         // { dg-error "ambiguous" }
    sink_5_23678(source());  // { dg-error "ambiguous" }
    return 0;
}

two   sink_5_24567(const          A&);  // { dg-error "" }
four  sink_5_24567(const volatile A&);  // { dg-error "" }
five  sink_5_24567(               A&&);  // { dg-error "" }
six   sink_5_24567(const          A&&);  // { dg-error "" }
seven sink_5_24567(volatile       A&&);  // { dg-error "" }

int test5_24567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_24567(cv_source());  // { dg-error "no match" }
    return 0;
}

two   sink_5_24678(const          A&);  // { dg-error "" }
four  sink_5_24678(const volatile A&);
six   sink_5_24678(const          A&&);  // { dg-error "" }
seven sink_5_24678(volatile       A&&);  // { dg-error "" }
eight sink_5_24678(const volatile A&&);  // { dg-error "" }

int test5_24678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_24678(source());  // { dg-error "ambiguous" }
    return 0;
}

three sink_5_34567(volatile       A&);  // { dg-error "" }
four  sink_5_34567(const volatile A&);  // { dg-error "" }
five  sink_5_34567(               A&&);  // { dg-error "" }
six   sink_5_34567(const          A&&);  // { dg-error "" }
seven sink_5_34567(volatile       A&&);  // { dg-error "" }

int test5_34567()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_34567(cv_source());  // { dg-error "no match" }
    return 0;
}

three sink_5_34678(volatile       A&);
four  sink_5_34678(const volatile A&);
six   sink_5_34678(const          A&&);  // { dg-error "" }
seven sink_5_34678(volatile       A&&);  // { dg-error "" }
eight sink_5_34678(const volatile A&&);  // { dg-error "" }

int test5_34678()
{
                   A a;
    const          A ca = a;
          volatile A va;
    const volatile A cva = a;
    sink_5_34678(source());  // { dg-error "ambiguous" }
    return 0;
}

int main()
{
    return test5_12356() + test5_12357() + test5_12367() + test5_12467() +
           test5_12567() + test5_12678() + test5_13467() + test5_13567() +
           test5_13678() + test5_13678() + test5_23456() + test5_23457() +
           test5_23458() + test5_23467() + test5_23468() + test5_23478() +
           test5_23567() + test5_23568() + test5_23578() + test5_23678() +
           test5_24678() + test5_34678();
}
