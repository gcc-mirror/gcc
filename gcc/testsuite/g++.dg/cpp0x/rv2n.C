// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile { target c++11 } }
// { dg-options "" }

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

// 2 at a time

one   sink_2_12(               A&);
two   sink_2_12(const          A&);

int test2_12()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_12(va);           // { dg-error "" }
    sink_2_12(cva);          // { dg-error "" }
    sink_2_12(v_source());   // { dg-error "" }
    sink_2_12(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_2_13(               A&);
three sink_2_13(volatile       A&);

int test2_13()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_13(ca);           // { dg-error "" }
    sink_2_13(cva);          // { dg-error "" }
    sink_2_13(source());     // { dg-error "" }
    sink_2_13(c_source());   // { dg-error "" }
    sink_2_13(v_source());   // { dg-error "" }
    sink_2_13(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_2_14(               A&);
four  sink_2_14(const volatile A&);

int test2_14()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_14(source());     // { dg-error "" }
    sink_2_14(c_source());   // { dg-error "" }
    sink_2_14(v_source());   // { dg-error "" }
    sink_2_14(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_2_15(               A&);
five  sink_2_15(               A&&);

int test2_15()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
   sink_2_15(ca);           // { dg-error "" }
   sink_2_15(va);           // { dg-error "" }
   sink_2_15(cva);          // { dg-error "" }
   sink_2_15(c_source());   // { dg-error "" }
   sink_2_15(v_source());   // { dg-error "" }
   sink_2_15(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_2_16(               A&);
six   sink_2_16(const          A&&);

int test2_16()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_16(ca);	     // { dg-error "" }
    sink_2_16(va);           // { dg-error "" }
    sink_2_16(cva);          // { dg-error "" }
    sink_2_16(v_source());   // { dg-error "" }
    sink_2_16(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_2_17(               A&);
seven sink_2_17(volatile       A&&);

int test2_17()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_17(ca);           // { dg-error "" }
    sink_2_17(va);           // { dg-error "" }
    sink_2_17(cva);          // { dg-error "" }
    sink_2_17(c_source());   // { dg-error "" }
    sink_2_17(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_2_18(               A&);
eight sink_2_18(const volatile A&&);

int test2_18()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_18(ca);		// { dg-error "" }
    sink_2_18(va);		// { dg-error "" }
    sink_2_18(cva);		// { dg-error "" }
}

two   sink_2_23(const          A&);
three sink_2_23(volatile       A&);

int test2_23()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_23(a);            // { dg-error "ambiguous" }
    sink_2_23(cva);          // { dg-error "" }
    sink_2_23(v_source());   // { dg-error "" }
    sink_2_23(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_2_24(const          A&);
four  sink_2_24(const volatile A&);

int test2_24()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_24(v_source());   // { dg-error "" }
    sink_2_24(cv_source());  // { dg-error "" }
    return 0;
}

three sink_2_34(volatile       A&);
four  sink_2_34(const volatile A&);

int test2_34()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_34(source());     // { dg-error "" }
    sink_2_34(c_source());   // { dg-error "" }
    sink_2_34(v_source());   // { dg-error "" }
    sink_2_34(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_2_25(const          A&);
five  sink_2_25(               A&&);

int test2_25()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
   sink_2_25(va);           // { dg-error "" }
   sink_2_25(cva);          // { dg-error "" }
   sink_2_25(v_source());   // { dg-error "" }
   sink_2_25(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_2_26(const          A&);
six   sink_2_26(const          A&&);

int test2_26()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_26(va);           // { dg-error "" }
    sink_2_26(cva);          // { dg-error "" }
    sink_2_26(v_source());   // { dg-error "" }
    sink_2_26(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_2_27(const          A&);
seven sink_2_27(volatile       A&&);

int test2_27()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_27(va);	     // { dg-error "" }
    sink_2_27(cva);          // { dg-error "" }
    sink_2_27(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_2_28(const          A&);
eight sink_2_28(const volatile A&&);

int test2_28()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_28(va);		// { dg-error "" }
    sink_2_28(cva);		// { dg-error "" }
}

three sink_2_35(volatile       A&);
five  sink_2_35(               A&&);

int test2_35()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_35(ca);           // { dg-error "" }
    sink_2_35(cva);          // { dg-error "" }
    sink_2_35(c_source());   // { dg-error "" }
    sink_2_35(v_source());   // { dg-error "" }
    sink_2_35(cv_source());  // { dg-error "" }
    return 0;
}

three sink_2_36(volatile       A&);
six   sink_2_36(const          A&&);

int test2_36()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_36(ca);		// { dg-error "" }
    sink_2_36(cva);          // { dg-error "" }
    sink_2_36(v_source());   // { dg-error "" }
    sink_2_36(cv_source());  // { dg-error "" }
    return 0;
}

three sink_2_37(volatile       A&);
seven sink_2_37(volatile       A&&);

int test2_37()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_37(ca);           // { dg-error "" }
    sink_2_37(cva);          // { dg-error "" }
    sink_2_37(c_source());   // { dg-error "" }
    sink_2_37(cv_source());  // { dg-error "" }
    return 0;
}

three sink_2_38(volatile       A&);
eight sink_2_38(const volatile A&&);

int test2_38()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_38(ca);		// { dg-error "" }
    sink_2_38(cva);		// { dg-error "" }
}

four  sink_2_45(const volatile A&);
five  sink_2_45(               A&&);

int test2_45()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_45(c_source());   // { dg-error "" }
    sink_2_45(v_source());   // { dg-error "" }
    sink_2_45(cv_source());  // { dg-error "" }
    return 0;
}

four  sink_2_46(const volatile A&);
six   sink_2_46(const          A&&);

int test2_46()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_46(v_source());   // { dg-error "" }
    sink_2_46(cv_source());  // { dg-error "" }
    return 0;
}

four  sink_2_47(const volatile A&);
seven sink_2_47(volatile       A&&);

int test2_47()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_47(c_source());   // { dg-error "" }
    sink_2_47(cv_source());  // { dg-error "" }
    return 0;
}

five  sink_2_56(               A&&);
six   sink_2_56(const          A&&);

int test2_56()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_56(a);		// { dg-error "" }
    sink_2_56(ca);		// { dg-error "" }
    sink_2_56(va);           // { dg-error "" }
    sink_2_56(cva);          // { dg-error "" }
    sink_2_56(v_source());   // { dg-error "" }
    sink_2_56(cv_source());  // { dg-error "" }
    return 0;
}

five  sink_2_57(               A&&);
seven sink_2_57(volatile       A&&);

int test2_57()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_57(a);		// { dg-error "" }
    sink_2_57(va);		// { dg-error "" }
    sink_2_57(ca);           // { dg-error "" }
    sink_2_57(cva);          // { dg-error "" }
    sink_2_57(c_source());   // { dg-error "" }
    sink_2_57(cv_source());  // { dg-error "" }
    return 0;
}

five  sink_2_58(               A&&);
eight sink_2_58(const volatile A&&);

int test2_58()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_58(a);		// { dg-error "" }
    sink_2_58(ca);		// { dg-error "" }
    sink_2_58(va);		// { dg-error "" }
    sink_2_58(cva);		// { dg-error "" }
}

six   sink_2_67(const          A&&);
seven sink_2_67(volatile       A&&);

int test2_67()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_67(a);            // { dg-error "" }
    sink_2_67(ca);	     // { dg-error "" }
    sink_2_67(va);	     // { dg-error "" }
    sink_2_67(cva);          // { dg-error "" }
    sink_2_67(source());     // { dg-error "" }
    sink_2_67(cv_source());  // { dg-error "" }
    return 0;
}

six   sink_2_68(const          A&&);
eight sink_2_68(const volatile A&&);

int test2_68()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_68(a);		// { dg-error "" }
    sink_2_68(ca);		// { dg-error "" }
    sink_2_68(va);		// { dg-error "" }
    sink_2_68(cva);		// { dg-error "" }
}

seven sink_2_78(volatile       A&&);
eight sink_2_78(const volatile A&&);

int test2_78()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_2_78(a);		// { dg-error "" }
    sink_2_78(ca);		// { dg-error "" }
    sink_2_78(va);		// { dg-error "" }
    sink_2_78(cva);		// { dg-error "" }
}

int main()
{
    return test2_12() + test2_13() + test2_15() + test2_16() +
           test2_17() + test2_23() + test2_25() + test2_26() +
           test2_27() + test2_35() + test2_36() + test2_37() +
           test2_56() + test2_57() + test2_67();
}
