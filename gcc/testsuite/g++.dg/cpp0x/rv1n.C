// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile { target c++11 } }

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

one   sink_1_1(               A&);  // { dg-message "" }

int test1_1()
{
                   A a;
    const          A ca = a;	// { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a;	// { dg-error "deleted" }
    sink_1_1(ca);           // { dg-error "invalid initialization" }
    sink_1_1(va);           // { dg-error "invalid initialization" }
    sink_1_1(cva);          // { dg-error "invalid initialization" }
    sink_1_1(source());     // { dg-error "invalid initialization" }
    sink_1_1(c_source());   // { dg-error "invalid initialization" }
    sink_1_1(v_source());   // { dg-error "invalid initialization" }
    sink_1_1(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

two   sink_1_2(const          A&);  // { dg-message "" }

int test1_2()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_2(va);           // { dg-error "invalid initialization" }
    sink_1_2(cva);          // { dg-error "invalid initialization" }
    sink_1_2(v_source());   // { dg-error "invalid initialization" }
    sink_1_2(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

three sink_1_3(volatile       A&);  // { dg-message "" }

int test1_3()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_3(ca);           // { dg-error "invalid initialization" }
    sink_1_3(cva);          // { dg-error "invalid initialization" }
    sink_1_3(source());     // { dg-error "invalid initialization" }
    sink_1_3(c_source());   // { dg-error "invalid initialization" }
    sink_1_3(v_source());   // { dg-error "invalid initialization" }
    sink_1_3(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

four  sink_1_4(const volatile A&);  // { dg-message "" }

int test1_4()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_4(source());     // { dg-error "invalid initialization" }
    sink_1_4(c_source());   // { dg-error "invalid initialization" }
    sink_1_4(v_source());   // { dg-error "invalid initialization" }
    sink_1_4(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

five  sink_1_5(               A&&);  // { dg-message "" }

int test1_5()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_5(a);		// { dg-error "lvalue" }
    sink_1_5(ca);           // { dg-error "invalid initialization" }
    sink_1_5(va);           // { dg-error "invalid initialization" }
    sink_1_5(cva);          // { dg-error "invalid initialization" }
    sink_1_5(c_source());   // { dg-error "invalid initialization" }
    sink_1_5(v_source());   // { dg-error "invalid initialization" }
    sink_1_5(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

six   sink_1_6(const          A&&);  // { dg-message "" }

int test1_6()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_6(a);		// { dg-error "lvalue" }
    sink_1_6(ca);		// { dg-error "lvalue" }
    sink_1_6(va);           // { dg-error "invalid initialization" }
    sink_1_6(cva);          // { dg-error "invalid initialization" }
    sink_1_6(v_source());   // { dg-error "invalid initialization" }
    sink_1_6(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

seven sink_1_7(volatile       A&&);  // { dg-message "" }

int test1_7()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_7(a);	    // { dg-error "lvalue" }
    sink_1_7(ca);           // { dg-error "invalid initialization" }
    sink_1_7(va);	    // { dg-error "lvalue" }
    sink_1_7(cva);          // { dg-error "invalid initialization" }
    sink_1_7(c_source());   // { dg-error "invalid initialization" }
    sink_1_7(cv_source());  // { dg-error "invalid initialization" }
    return 0;
}

eight sink_1_8(const volatile A&&); // { dg-message "" }

int test1_8()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_1_8(a);		// { dg-error "lvalue" }
    sink_1_8(ca);		// { dg-error "lvalue" }
    sink_1_8(va);		// { dg-error "lvalue" }
    sink_1_8(cva);		// { dg-error "lvalue" }
    return 0;
}

int main()
{
    return test1_1() + test1_2() + test1_3() + test1_5() +
           test1_6() + test1_7();
}
