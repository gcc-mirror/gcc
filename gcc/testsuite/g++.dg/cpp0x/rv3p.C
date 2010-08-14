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

// 3 at a time

one   sink_3_123(               A&);
two   sink_3_123(const          A&);
three sink_3_123(volatile       A&);

int test3_123()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_123(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_123(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_123(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_123(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_3_123(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_3_124(               A&);
two   sink_3_124(const          A&);
four  sink_3_124(const volatile A&);

int test3_124()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_124(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_124(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_124(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_124(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_124(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_3_124(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_3_125(               A&);
two   sink_3_125(const          A&);
five  sink_3_125(               A&&);

int test3_125()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_125(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_125(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_125(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_125(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

one   sink_3_126(               A&);
two   sink_3_126(const          A&);
six   sink_3_126(const          A&&);

int test3_126()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_126(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_126(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_126(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_126(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_3_127(               A&);
two   sink_3_127(const          A&);
seven sink_3_127(volatile       A&&);

int test3_127()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_127(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_127(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_127(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_127(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_3_127(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_3_128(               A&);
two   sink_3_128(const          A&);
eight sink_3_128(const volatile A&&);

int test3_128()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_128(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_128(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_128(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_3_128(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_128(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_128(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_3_134(               A&);
three sink_3_134(volatile       A&);
four  sink_3_134(const volatile A&);

int test3_134()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_134(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_134(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_134(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_134(cva))         == 4 * sizeof(long)> t4;
    return 0;
}

one   sink_3_135(               A&);
three sink_3_135(volatile       A&);
five  sink_3_135(               A&&);

int test3_135()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_135(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_135(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_135(source()))    == 5 * sizeof(long)> t5;
    return 0;
}

one   sink_3_136(               A&);
three sink_3_136(volatile       A&);
six   sink_3_136(const          A&&);

int test3_136()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_136(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_136(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_136(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_136(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_3_137(               A&);
three sink_3_137(volatile       A&);
seven sink_3_137(volatile       A&&);

int test3_137()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_137(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_137(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_137(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_137(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_3_138(               A&);
three sink_3_138(volatile       A&);
eight sink_3_138(const volatile A&&);

int test3_138()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_138(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_138(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_138(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_3_138(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_138(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_138(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_3_145(               A&);
four  sink_3_145(const volatile A&);
five  sink_3_145(               A&&);

int test3_145()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_145(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_145(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_145(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_145(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_145(source()))    == 5 * sizeof(long)> t5;
    return 0;
}

one   sink_3_146(               A&);
four  sink_3_146(const volatile A&);
six   sink_3_146(const          A&&);

int test3_146()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_146(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_146(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_146(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_146(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_146(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_146(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_3_147(               A&);
four  sink_3_147(const volatile A&);
seven sink_3_147(volatile       A&&);

int test3_147()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_147(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_147(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_147(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_147(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_147(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_147(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_3_148(               A&);
four  sink_3_148(const volatile A&);
eight sink_3_148(const volatile A&&);

int test3_148()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_148(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_148(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_148(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_148(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_148(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_3_148(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_148(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_148(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_3_156(               A&);
five  sink_3_156(               A&&);
six   sink_3_156(const          A&&);

int test3_156()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_156(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_156(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_156(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

one   sink_3_157(               A&);
five  sink_3_157(               A&&);
seven sink_3_157(volatile       A&&);

int test3_157()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_157(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_157(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_157(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_3_158(               A&);
five  sink_3_158(               A&&);
eight sink_3_158(const volatile A&&);

int test3_158()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_158(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_158(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_158(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_158(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_158(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_3_167(               A&);
six   sink_3_167(const          A&&);
seven sink_3_167(volatile       A&&);

int test3_167()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_167(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_167(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_167(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

one   sink_3_168(               A&);
six   sink_3_168(const          A&&);
eight sink_3_168(const volatile A&&);

int test3_168()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_168(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_168(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_168(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_168(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_168(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

one   sink_3_178(               A&);
seven sink_3_178(volatile       A&&);
eight sink_3_178(const volatile A&&);

int test3_178()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_178(a))           == 1 * sizeof(long)> t1;
    sa<sizeof(sink_3_178(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_178(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_178(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_3_178(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_3_234(const          A&);
three sink_3_234(volatile       A&);
four  sink_3_234(const volatile A&);

int test3_234()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_234(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_234(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_234(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_234(source()))    == 2 * sizeof(long)> t5;
    sa<sizeof(sink_3_234(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

two   sink_3_235(const          A&);
three sink_3_235(volatile       A&);
five  sink_3_235(               A&&);

int test3_235()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_235(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_235(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_235(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_235(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

two   sink_3_236(const          A&);
three sink_3_236(volatile       A&);
six   sink_3_236(const          A&&);

int test3_236()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_236(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_236(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_236(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_236(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_3_237(const          A&);
three sink_3_237(volatile       A&);
seven sink_3_237(volatile       A&&);

int test3_237()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_237(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_237(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_237(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_237(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_3_237(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_3_238(const          A&);
three sink_3_238(volatile       A&);
eight sink_3_238(const volatile A&&);

int test3_238()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_238(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_238(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_238(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_3_238(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_238(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_238(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_3_245(const          A&);
four  sink_3_245(const volatile A&);
five  sink_3_245(               A&&);

int test3_245()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_245(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_245(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_245(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_245(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_245(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_245(c_source()))  == 2 * sizeof(long)> t6;
    return 0;
}

two   sink_3_246(const          A&);
four  sink_3_246(const volatile A&);
six   sink_3_246(const          A&&);

int test3_246()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_246(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_246(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_246(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_246(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_246(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_246(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_3_247(const          A&);
four  sink_3_247(const volatile A&);
seven sink_3_247(volatile       A&&);

int test3_247()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_247(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_247(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_247(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_247(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_247(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_247(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_3_247(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_3_248(const          A&);
four  sink_3_248(const volatile A&);
eight sink_3_248(const volatile A&&);

int test3_248()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_248(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_248(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_248(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_248(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_248(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_3_248(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_248(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_248(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_3_256(const          A&);
five  sink_3_256(               A&&);
six   sink_3_256(const          A&&);

int test3_256()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_256(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_256(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_256(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_256(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

two   sink_3_257(const          A&);
five  sink_3_257(               A&&);
seven sink_3_257(volatile       A&&);

int test3_257()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_257(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_257(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_257(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_257(c_source()))  == 2 * sizeof(long)> t6;
    sa<sizeof(sink_3_257(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_3_258(const          A&);
five  sink_3_258(               A&&);
eight sink_3_258(const volatile A&&);

int test3_258()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_258(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_258(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_258(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_258(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_258(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_258(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_3_267(const          A&);
six   sink_3_267(const          A&&);
seven sink_3_267(volatile       A&&);

int test3_267()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_267(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_267(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_267(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_267(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

two   sink_3_268(const          A&);
six   sink_3_268(const          A&&);
eight sink_3_268(const volatile A&&);

int test3_268()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_268(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_268(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_268(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_268(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_268(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_268(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

two   sink_3_278(const          A&);
seven sink_3_278(volatile       A&&);
eight sink_3_278(const volatile A&&);

int test3_278()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_278(a))           == 2 * sizeof(long)> t1;
    sa<sizeof(sink_3_278(ca))          == 2 * sizeof(long)> t2;
    sa<sizeof(sink_3_278(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_278(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_278(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_3_278(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_3_345(volatile       A&);
four  sink_3_345(const volatile A&);
five  sink_3_345(               A&&);

int test3_345()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_345(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_345(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_345(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_345(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_345(source()))    == 5 * sizeof(long)> t5;
    return 0;
}

three sink_3_346(volatile       A&);
four  sink_3_346(const volatile A&);
six   sink_3_346(const          A&&);

int test3_346()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_346(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_346(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_346(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_346(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_346(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_346(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

three sink_3_347(volatile       A&);
four  sink_3_347(const volatile A&);
seven sink_3_347(volatile       A&&);

int test3_347()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_347(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_347(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_347(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_347(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_347(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_347(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_3_348(volatile       A&);
four  sink_3_348(const volatile A&);
eight sink_3_348(const volatile A&&);

int test3_348()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_348(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_348(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_348(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_348(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_348(source()))    == 8 * sizeof(long)> t5;
    sa<sizeof(sink_3_348(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_348(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_348(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_3_356(volatile       A&);
five  sink_3_356(               A&&);
six   sink_3_356(const          A&&);

int test3_356()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_356(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_356(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_356(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_356(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

three sink_3_357(volatile       A&);
five  sink_3_357(               A&&);
seven sink_3_357(volatile       A&&);

int test3_357()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_357(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_357(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_357(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_357(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_3_358(volatile       A&);
five  sink_3_358(               A&&);
eight sink_3_358(const volatile A&&);

int test3_358()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_358(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_358(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_358(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_358(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_358(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_358(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_3_367(volatile       A&);
six   sink_3_367(const          A&&);
seven sink_3_367(volatile       A&&);

int test3_367()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_367(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_367(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_367(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_367(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

three sink_3_368(volatile       A&);
six   sink_3_368(const          A&&);
eight sink_3_368(const volatile A&&);

int test3_368()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_368(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_368(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_368(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_368(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_368(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_368(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

three sink_3_378(volatile       A&);
seven sink_3_378(volatile       A&&);
eight sink_3_378(const volatile A&&);

int test3_378()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_378(a))           == 3 * sizeof(long)> t1;
    sa<sizeof(sink_3_378(va))          == 3 * sizeof(long)> t3;
    sa<sizeof(sink_3_378(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_378(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_378(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_3_378(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_3_456(const volatile A&);
five  sink_3_456(               A&&);
six   sink_3_456(const          A&&);

int test3_456()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_456(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_3_456(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_456(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_456(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_456(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_456(c_source()))  == 6 * sizeof(long)> t6;
    return 0;
}

four  sink_3_457(const volatile A&);
five  sink_3_457(               A&&);
seven sink_3_457(volatile       A&&);

int test3_457()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_457(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_3_457(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_457(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_457(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_457(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_457(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

four  sink_3_458(const volatile A&);
five  sink_3_458(               A&&);
eight sink_3_458(const volatile A&&);

int test3_458()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_458(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_3_458(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_458(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_458(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_458(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_458(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_458(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_458(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_3_467(const volatile A&);
six   sink_3_467(const          A&&);
seven sink_3_467(volatile       A&&);

int test3_467()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_467(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_3_467(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_467(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_467(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_467(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_467(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

four  sink_3_468(const volatile A&);
six   sink_3_468(const          A&&);
eight sink_3_468(const volatile A&&);

int test3_468()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_468(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_3_468(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_468(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_468(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_468(source()))    == 6 * sizeof(long)> t5;
    sa<sizeof(sink_3_468(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_468(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_468(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

four  sink_3_478(const volatile A&);
seven sink_3_478(volatile       A&&);
eight sink_3_478(const volatile A&&);

int test3_478()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_478(a))           == 4 * sizeof(long)> t1;
    sa<sizeof(sink_3_478(ca))          == 4 * sizeof(long)> t2;
    sa<sizeof(sink_3_478(va))          == 4 * sizeof(long)> t3;
    sa<sizeof(sink_3_478(cva))         == 4 * sizeof(long)> t4;
    sa<sizeof(sink_3_478(source()))    == 7 * sizeof(long)> t5;
    sa<sizeof(sink_3_478(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_478(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_3_478(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

five  sink_3_567(               A&&);
six   sink_3_567(const          A&&);
seven sink_3_567(volatile       A&&);

int test3_567()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_567(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_567(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_567(v_source()))  == 7 * sizeof(long)> t7;
    return 0;
}

five  sink_3_568(               A&&);
six   sink_3_568(const          A&&);
eight sink_3_568(const volatile A&&);

int test3_568()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_568(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_568(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_568(v_source()))  == 8 * sizeof(long)> t7;
    sa<sizeof(sink_3_568(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

five  sink_3_578(               A&&);
seven sink_3_578(volatile       A&&);
eight sink_3_578(const volatile A&&);

int test3_578()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_578(source()))    == 5 * sizeof(long)> t5;
    sa<sizeof(sink_3_578(c_source()))  == 8 * sizeof(long)> t6;
    sa<sizeof(sink_3_578(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_3_578(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

six   sink_3_678(const          A&&);
seven sink_3_678(volatile       A&&);
eight sink_3_678(const volatile A&&);

int test3_678()
{
                   A a;
    const          A ca;
          volatile A va;
    const volatile A cva;
    sa<sizeof(sink_3_678(c_source()))  == 6 * sizeof(long)> t6;
    sa<sizeof(sink_3_678(v_source()))  == 7 * sizeof(long)> t7;
    sa<sizeof(sink_3_678(cv_source())) == 8 * sizeof(long)> t8;
    return 0;
}

int main()
{
    return test3_123() + test3_124() + test3_125() + test3_126() +
           test3_127() + test3_128() + test3_134() + test3_135() +
           test3_136() + test3_137() + test3_138() + test3_145() +
           test3_146() + test3_147() + test3_148() + test3_156() +
           test3_157() + test3_158() + test3_167() + test3_168() +
           test3_178() + test3_234() + test3_235() + test3_236() +
           test3_237() + test3_238() + test3_245() + test3_246() +
           test3_247() + test3_248() + test3_256() + test3_257() +
           test3_258() + test3_267() + test3_268() + test3_278() +
           test3_345() + test3_346() + test3_347() + test3_348() +
           test3_356() + test3_357() + test3_358() + test3_367() +
           test3_368() + test3_378() + test3_456() + test3_457() +
           test3_458() + test3_467() + test3_468() + test3_478() +
           test3_567() + test3_568() + test3_578() + test3_678();
}
