// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

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

// 2 at a time

one   sink_2_12(               A&);
two   sink_2_12(const          A&);

int test2_12()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_12(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_12(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_12(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_2_12(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_2_13(               A&);
three sink_2_13(volatile       A&);

int test2_13()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_13(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_13(va))          == 3 * sizeof(long)> t3;
    return 0;
}

one   sink_2_14(               A&);
four  sink_2_14(const volatile A&);

int test2_14()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_14(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_14(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_2_14(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_2_14(cva))         == 4 * sizeof(long)> t4;
    return 0;
}

one   sink_2_15(               A&);
five  sink_2_15(               A&&);

int test2_15()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_15(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_15(source()))    == 5 * sizeof(long)> t5;
    return 0;
}

one   sink_2_16(               A&);
six   sink_2_16(const          A&&);

int test2_16()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_16(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_16(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_2_16(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_2_17(               A&);
seven sink_2_17(volatile       A&&);

int test2_17()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_17(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_17(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_2_17(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_2_18(               A&);
eight sink_2_18(const volatile A&&);

int test2_18()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_18(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_2_18(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_2_18(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_2_18(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_2_18(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_2_23(const          A&);
three sink_2_23(volatile       A&);

int test2_23()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_23(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_23(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_2_23(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_2_23(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

two   sink_2_24(const          A&);
four  sink_2_24(const volatile A&);

int test2_24()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_24(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_2_24(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_24(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_2_24(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_2_24(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_2_24(c_source()))  == 2 * sizeof(long)> t6;
//    sa<sizeof(sink_2_24(v_source()))  == 4 * sizeof(long)> t7;
//    sa<sizeof(sink_2_24(cv_source())) == 4 * sizeof(long)> t8;
    return 0;
}

two   sink_2_25(const          A&);
five  sink_2_25(               A&&);

int test2_25()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_25(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_2_25(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_25(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_2_25(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

two   sink_2_26(const          A&);
six   sink_2_26(const          A&&);

int test2_26()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_26(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_2_26(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_26(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_2_26(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_2_27(const          A&);
seven sink_2_27(volatile       A&&);

int test2_27()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_27(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_2_27(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_27(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_2_27(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_2_27(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_2_28(const          A&);
eight sink_2_28(const volatile A&&);

int test2_28()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_28(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_2_28(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_2_28(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_2_28(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_2_28(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_2_28(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_2_34(volatile       A&);
four  sink_2_34(const volatile A&);

int test2_34()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_34(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_2_34(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_2_34(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_2_34(cva))         == 4 * sizeof(long)> t4;
//    sa<sizeof(sink_2_34(source()))    == 4 * sizeof(long)> t5;
//    sa<sizeof(sink_2_34(c_source()))  == 4 * sizeof(long)> t6;
//    sa<sizeof(sink_2_34(v_source()))  == 4 * sizeof(long)> t7;
//    sa<sizeof(sink_2_34(cv_source())) == 4 * sizeof(long)> t8;
    return 0;
}

three sink_2_35(volatile       A&);
five  sink_2_35(               A&&);

int test2_35()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_35(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_2_35(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_2_35(source()))    == 5 * sizeof(long)> t5;
    return 0;
}

three sink_2_36(volatile       A&);
six   sink_2_36(const          A&&);

int test2_36()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_36(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_2_36(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_2_36(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_2_36(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

three sink_2_37(volatile       A&);
seven sink_2_37(volatile       A&&);

int test2_37()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_37(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_2_37(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_2_37(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_2_37(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_2_38(volatile       A&);
eight sink_2_38(const volatile A&&);

int test2_38()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_38(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_2_38(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_2_38(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_2_38(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_2_38(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_2_38(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_2_45(const volatile A&);
five  sink_2_45(               A&&);

int test2_45()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_45(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_2_45(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_2_45(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_2_45(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_2_45(source()))    == 5 * sizeof(long)> t5;
//    sa<sizeof(sink_2_45(c_source()))  == 4 * sizeof(long)> t6;
//    sa<sizeof(sink_2_45(v_source()))  == 4 * sizeof(long)> t7;
//    sa<sizeof(sink_2_45(cv_source())) == 4 * sizeof(long)> t8;
    return 0;
}

four  sink_2_46(const volatile A&);
six   sink_2_46(const          A&&);

int test2_46()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_46(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_2_46(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_2_46(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_2_46(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_2_46(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_2_46(c_source()))  == 6 * sizeof(long)> t6;
//    sa<sizeof(sink_2_46(v_source()))  == 4 * sizeof(long)> t7;
//    sa<sizeof(sink_2_46(cv_source())) == 4 * sizeof(long)> t8;
    return 0;
}

four  sink_2_47(const volatile A&);
seven sink_2_47(volatile       A&&);

int test2_47()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_47(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_2_47(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_2_47(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_2_47(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_2_47(source()))    == 7 * sizeof(long)> t5;
//    sa<sizeof(sink_2_47(c_source()))  == 4 * sizeof(long)> t6;
    sa<sizeof(sink_2_47(v_source()))  == 7 * sizeof(long)> t7;
//    sa<sizeof(sink_2_47(cv_source())) == 4 * sizeof(long)> t8;
    return 0;
}

four  sink_2_48(const volatile A&);
eight sink_2_48(const volatile A&&);

int test2_48()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_48(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_2_48(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_2_48(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_2_48(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_2_48(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_2_48(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_2_48(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_2_48(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

five  sink_2_56(               A&&);
six   sink_2_56(const          A&&);

int test2_56()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_56(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_2_56(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

five  sink_2_57(               A&&);
seven sink_2_57(volatile       A&&);

int test2_57()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_57(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_2_57(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

five  sink_2_58(               A&&);
eight sink_2_58(const volatile A&&);

int test2_58()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_58(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_2_58(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_2_58(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_2_58(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

six   sink_2_67(const          A&&);
seven sink_2_67(volatile       A&&);

int test2_67()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_67(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_2_67(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

six   sink_2_68(const          A&&);
eight sink_2_68(const volatile A&&);

int test2_68()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_68(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_2_68(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_2_68(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_2_68(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

seven sink_2_78(volatile       A&&);
eight sink_2_78(const volatile A&&);

int test2_78()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_2_78(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_2_78(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_2_78(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_2_78(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test2_12() + test2_13() + test2_14() + test2_15() +
           test2_16() + test2_17() + test2_18() + test2_23() +
           test2_24() + test2_25() + test2_26() + test2_27() +
           test2_28() + test2_34() + test2_35() + test2_36() +
           test2_37() + test2_38() + test2_45() + test2_46() +
           test2_47() + test2_48() + test2_56() + test2_57() +
           test2_58() + test2_67() + test2_68() + test2_78();
}
