// I, Howard Hinnant, hereby place this code in the public domain.

// Test overload resolution among reference types

// { dg-do compile { target c++11 } }
// { dg-options "-fno-ipa-icf" }

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
      volatile A  v_source(); // { dg-warning "deprecated" "" { target c++2a } }
const volatile A cv_source(); // { dg-warning "deprecated" "" { target c++2a } }

// 3 at a time

one   sink_3_123(               A&);
two   sink_3_123(const          A&);
three sink_3_123(volatile       A&);

int test3_123()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_123(cva);          // { dg-error "" }
    sink_3_123(v_source());   // { dg-error "" }
    sink_3_123(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_125(               A&);
two   sink_3_125(const          A&);
five  sink_3_125(               A&&);

one   sink_3_124(               A&);
two   sink_3_124(const          A&);
four  sink_3_124(const volatile A&);

int test3_124()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_124(v_source());   // { dg-error "" }
    sink_3_124(cv_source());  // { dg-error "" }
    return 0;
}

int test3_125()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_125(va);           // { dg-error "" }
    sink_3_125(cva);          // { dg-error "" }
    sink_3_125(v_source());   // { dg-error "" }
    sink_3_125(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_126(               A&);
two   sink_3_126(const          A&);
six   sink_3_126(const          A&&);

int test3_126()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_126(va);           // { dg-error "" }
    sink_3_126(cva);          // { dg-error "" }
    sink_3_126(v_source());   // { dg-error "" }
    sink_3_126(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_127(               A&);
two   sink_3_127(const          A&);
seven sink_3_127(volatile       A&&);

int test3_127()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_127(va);		// { dg-error "" }
    sink_3_127(cva);          // { dg-error "" }
    sink_3_127(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_128(               A&);
two   sink_3_128(const          A&);
eight sink_3_128(const volatile A&&);

int test3_128()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }

    sink_3_128(va);		// { dg-error "" }
    sink_3_128(cva);		// { dg-error "" }
    return 0;
}

one   sink_3_134(               A&);
three sink_3_134(volatile       A&);
four  sink_3_134(const volatile A&);

int test3_134()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_134(source());     // { dg-error "" }
    sink_3_134(c_source());   // { dg-error "" }
    sink_3_134(v_source());   // { dg-error "" }
    sink_3_134(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_135(               A&);
three sink_3_135(volatile       A&);
five  sink_3_135(               A&&);

int test3_135()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_135(ca);           // { dg-error "" }
    sink_3_135(cva);          // { dg-error "" }
    sink_3_135(c_source());   // { dg-error "" }
    sink_3_135(v_source());   // { dg-error "" }
    sink_3_135(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_136(               A&);
three sink_3_136(volatile       A&);
six   sink_3_136(const          A&&);

int test3_136()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_136(ca);		// { dg-error "" }
    sink_3_136(cva);          // { dg-error "" }
    sink_3_136(v_source());   // { dg-error "" }
    sink_3_136(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_137(               A&);
three sink_3_137(volatile       A&);
seven sink_3_137(volatile       A&&);

int test3_137()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_137(ca);           // { dg-error "" }
    sink_3_137(cva);          // { dg-error "" }
    sink_3_137(c_source());   // { dg-error "" }
    sink_3_137(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_138(               A&);
three sink_3_138(volatile       A&);
eight sink_3_138(const volatile A&&);

int test3_138()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_138(ca);		// { dg-error "" }
    sink_3_138(cva);		// { dg-error "" }
    return 0;
}

one   sink_3_145(               A&);
four  sink_3_145(const volatile A&);
five  sink_3_145(               A&&);

int test3_145()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_145(c_source());   // { dg-error "" }
    sink_3_145(v_source());   // { dg-error "" }
    sink_3_145(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_146(               A&);
four  sink_3_146(const volatile A&);
six   sink_3_146(const          A&&);

int test3_146()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_146(v_source());   // { dg-error "" }
    sink_3_146(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_147(               A&);
four  sink_3_147(const volatile A&);
seven sink_3_147(volatile       A&&);

int test3_147()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_147(c_source());   // { dg-error "" }
    sink_3_147(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_156(               A&);
five  sink_3_156(               A&&);
six   sink_3_156(const          A&&);

int test3_156()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_156(ca);		// { dg-error "" }
    sink_3_156(va);           // { dg-error "" }
    sink_3_156(cva);          // { dg-error "" }
    sink_3_156(v_source());   // { dg-error "" }
    sink_3_156(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_157(               A&);
five  sink_3_157(               A&&);
seven sink_3_157(volatile       A&&);

int test3_157()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_157(ca);           // { dg-error "" }
    sink_3_157(va);	      // { dg-error "" }
    sink_3_157(cva);          // { dg-error "" }
    sink_3_157(c_source());   // { dg-error "" }
    sink_3_157(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_158(               A&);
five  sink_3_158(               A&&);
eight sink_3_158(const volatile A&&);

int test3_158()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_158(ca);		// { dg-error "" }
    sink_3_158(va);		// { dg-error "" }
    sink_3_158(cva);		// { dg-error "" }
    return 0;
}

one   sink_3_167(               A&);
six   sink_3_167(const          A&&);
seven sink_3_167(volatile       A&&);

int test3_167()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_167(ca);		// { dg-error "" }
    sink_3_167(va);		// { dg-error "" }
    sink_3_167(cva);          // { dg-error "" }
    sink_3_167(source());     // { dg-error "" }
    sink_3_167(cv_source());  // { dg-error "" }
    return 0;
}

one   sink_3_168(               A&);
six   sink_3_168(const          A&&);
eight sink_3_168(const volatile A&&);

int test3_168()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_168(ca);		// { dg-error "" }
    sink_3_168(va);		// { dg-error "" }
    sink_3_168(cva);		// { dg-error "" }
    return 0;
}

one   sink_3_178(               A&);
seven sink_3_178(volatile       A&&);
eight sink_3_178(const volatile A&&);

int test3_178()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_178(ca);		// { dg-error "" }
    sink_3_178(va);		// { dg-error "" }
    sink_3_178(cva);		// { dg-error "" }
    return 0;
}

two   sink_3_234(const          A&);
three sink_3_234(volatile       A&);
four  sink_3_234(const volatile A&);

int test3_234()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_234(a);            // { dg-error "" }
    sink_3_234(v_source());   // { dg-error "" }
    sink_3_234(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_235(const          A&);
three sink_3_235(volatile       A&);
five  sink_3_235(               A&&);

int test3_235()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_235(a);            // { dg-error "" }
    sink_3_235(cva);          // { dg-error "" }
    sink_3_235(v_source());   // { dg-error "" }
    sink_3_235(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_236(const          A&);
three sink_3_236(volatile       A&);
six   sink_3_236(const          A&&);

int test3_236()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_236(a);            // { dg-error "" }
    sink_3_236(cva);          // { dg-error "" }
    sink_3_236(v_source());   // { dg-error "" }
    sink_3_236(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_237(const          A&);
three sink_3_237(volatile       A&);
seven sink_3_237(volatile       A&&);

int test3_237()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_237(a);            // { dg-error "" }
    sink_3_237(cva);          // { dg-error "" }
    sink_3_237(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_238(const          A&);
three sink_3_238(volatile       A&);
eight sink_3_238(const volatile A&&);

int test3_238()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_238(a);  // { dg-error "" }
    sink_3_238(cva); // { dg-error "" }
    return 0;
}

two   sink_3_245(const          A&);
four  sink_3_245(const volatile A&);
five  sink_3_245(               A&&);

int test3_245()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_245(v_source());   // { dg-error "" }
    sink_3_245(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_246(const          A&);
four  sink_3_246(const volatile A&);
six   sink_3_246(const          A&&);

int test3_246()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_246(v_source());   // { dg-error "" }
    sink_3_246(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_247(const          A&);
four  sink_3_247(const volatile A&);
seven sink_3_247(volatile       A&&);

int test3_247()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_247(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_256(const          A&);
five  sink_3_256(               A&&);
six   sink_3_256(const          A&&);

int test3_256()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_256(va);           // { dg-error "" }
    sink_3_256(cva);          // { dg-error "" }
    sink_3_256(v_source());   // { dg-error "" }
    sink_3_256(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_257(const          A&);
five  sink_3_257(               A&&);
seven sink_3_257(volatile       A&&);

int test3_257()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_257(va);		// { dg-error "" }
    sink_3_257(cva);          // { dg-error "" }
    sink_3_257(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_258(const          A&);
five  sink_3_258(               A&&);
eight sink_3_258(const volatile A&&);

int test3_258()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_258(va);		// { dg-error "" }
    sink_3_258(cva);		// { dg-error "" }
    return 0;
}

two   sink_3_267(const          A&);
six   sink_3_267(const          A&&);
seven sink_3_267(volatile       A&&);

int test3_267()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_267(va);		// { dg-error "" }
    sink_3_267(cva);          // { dg-error "" }
    sink_3_267(source());     // { dg-error "" }
    sink_3_267(cv_source());  // { dg-error "" }
    return 0;
}

two   sink_3_268(const          A&);
six   sink_3_268(const          A&&);
eight sink_3_268(const volatile A&&);

int test3_268()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_268(va);		// { dg-error "" }
    sink_3_268(cva);		// { dg-error "" }
    return 0;
}

two   sink_3_278(const          A&);
seven sink_3_278(volatile       A&&);
eight sink_3_278(const volatile A&&);

int test3_278()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_278(va);		// { dg-error "" }
    sink_3_278(cva);		// { dg-error "" }
    return 0;
}

three sink_3_345(volatile       A&);
four  sink_3_345(const volatile A&);
five  sink_3_345(               A&&);

int test3_345()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_345(c_source());   // { dg-error "" }
    sink_3_345(v_source());   // { dg-error "" }
    sink_3_345(cv_source());  // { dg-error "" }
    return 0;
}

three sink_3_346(volatile       A&);
four  sink_3_346(const volatile A&);
six   sink_3_346(const          A&&);

int test3_346()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_346(v_source());   // { dg-error "" }
    sink_3_346(cv_source());  // { dg-error "" }
    return 0;
}

three sink_3_347(volatile       A&);
four  sink_3_347(const volatile A&);
seven sink_3_347(volatile       A&&);

int test3_347()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_347(c_source());   // { dg-error "" }
    sink_3_347(cv_source());  // { dg-error "" }
    return 0;
}

three sink_3_356(volatile       A&);
five  sink_3_356(               A&&);
six   sink_3_356(const          A&&);

int test3_356()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_356(ca);		// { dg-error "" }
    sink_3_356(cva);          // { dg-error "" }
    sink_3_356(v_source());   // { dg-error "" }
    sink_3_356(cv_source());  // { dg-error "" }
    return 0;
}

three sink_3_357(volatile       A&);
five  sink_3_357(               A&&);
seven sink_3_357(volatile       A&&);

int test3_357()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_357(ca);           // { dg-error "" }
    sink_3_357(cva);          // { dg-error "" }
    sink_3_357(c_source());   // { dg-error "" }
    sink_3_357(cv_source());  // { dg-error "" }
    return 0;
}

three sink_3_358(volatile       A&);
five  sink_3_358(               A&&);
eight sink_3_358(const volatile A&&);

int test3_358()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_358(ca);		// { dg-error "" }
    sink_3_358(cva);		// { dg-error "" }
    return 0;
}

three sink_3_367(volatile       A&);
six   sink_3_367(const          A&&);
seven sink_3_367(volatile       A&&);

int test3_367()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_367(ca);		// { dg-error "" }
    sink_3_367(cva);          // { dg-error "" }
    sink_3_367(source());     // { dg-error "" }
    sink_3_367(cv_source());  // { dg-error "" }
    return 0;
}

three sink_3_368(volatile       A&);
six   sink_3_368(const          A&&);
eight sink_3_368(const volatile A&&);

int test3_368()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_368(ca);		// { dg-error "" }
    sink_3_368(cva);		// { dg-error "" }
    return 0;
}

three sink_3_378(volatile       A&);
seven sink_3_378(volatile       A&&);
eight sink_3_378(const volatile A&&);

int test3_378()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_378(ca);		// { dg-error "" }
    sink_3_378(cva);		// { dg-error "" }
    return 0;
}

four  sink_3_456(const volatile A&);
five  sink_3_456(               A&&);
six   sink_3_456(const          A&&);

int test3_456()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_456(v_source());   // { dg-error "" }
    sink_3_456(cv_source());  // { dg-error "" }
    return 0;
}

four  sink_3_457(const volatile A&);
five  sink_3_457(               A&&);
seven sink_3_457(volatile       A&&);

int test3_457()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_457(c_source());   // { dg-error "" }
    sink_3_457(cv_source());  // { dg-error "" }
    return 0;
}

four  sink_3_467(const volatile A&);
six   sink_3_467(const          A&&);
seven sink_3_467(volatile       A&&);

int test3_467()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_467(source());     // { dg-error "" }
    sink_3_467(cv_source());  // { dg-error "" }
    return 0;
}

five  sink_3_567(               A&&);
six   sink_3_567(const          A&&);
seven sink_3_567(volatile       A&&);

int test3_567()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_567(a);		// { dg-error "" }
    sink_3_567(ca);		// { dg-error "" }
    sink_3_567(va);		// { dg-error "" }
    sink_3_567(cva);          // { dg-error "" }
    sink_3_567(cv_source());  // { dg-error "" }
    return 0;
}

five  sink_3_568(               A&&);
six   sink_3_568(const          A&&);
eight sink_3_568(const volatile A&&);

int test3_568()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_568(a);		// { dg-error "" }
    sink_3_568(ca);		// { dg-error "" }
    sink_3_568(va);		// { dg-error "" }
    sink_3_568(cva);		// { dg-error "" }
    return 0;
}

five  sink_3_578(               A&&);
seven sink_3_578(volatile       A&&);
eight sink_3_578(const volatile A&&);

int test3_578()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_578(a);		// { dg-error "" }
    sink_3_578(ca);		// { dg-error "" }
    sink_3_578(va);		// { dg-error "" }
    sink_3_578(cva);		// { dg-error "" }
    return 0;
}

six   sink_3_678(const          A&&);
seven sink_3_678(volatile       A&&);
eight sink_3_678(const volatile A&&);

int test3_678()
{
                   A a;
    const          A ca = a; // { dg-error "deleted" }
          volatile A va;
    const volatile A cva = a; // { dg-error "deleted" }
    sink_3_678(a);          // { dg-error "" }
    sink_3_678(ca);	    // { dg-error "" }
    sink_3_678(va);	    // { dg-error "" }
    sink_3_678(cva);	    // { dg-error "" }
    sink_3_678(source());   // { dg-error "" }
    return 0;
}

int main()
{
    return test3_123() + test3_125() + test3_126() + test3_127() +
           test3_135() + test3_136() + test3_137() + test3_156() +
           test3_157() + test3_167() + test3_234() + test3_235() +
           test3_236() + test3_237() + test3_238() + test3_256() +
           test3_257() + test3_267() + test3_356() + test3_357() +
           test3_367() + test3_467() + test3_567() + test3_678();
}
