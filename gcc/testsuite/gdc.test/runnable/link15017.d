// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/std15017variant.d

import imports.std15017variant;

void test()
{
    // OK <- in IndexExp::semantic
    Variant1[string] aa1;
    aa1["abc"] = Variant1();

    // already ok in CatExp::semantic with checkPostblit
    Variant2[] a2;
    a2 = a2 ~ Variant2();

    // already ok in CatAssignExp::semantic with checkPostblit
    Variant3[] a3;
    a3 ~= Variant3();

    // OK <- in CmpExp::semantic
    Variant4[] a4;
    assert(a4 < a4);

    // already OK in needDirectEq from EqualExp::semantic
    Variant5[] a5;
    assert(a5 == a5);

    // already OK in EqualExp::semantic
    Variant6[Variant7] aa67;
    assert(aa67 == aa67);

    // OK <- in InExp::semantic
    string[Variant8] aa8;
    assert(Variant8() in aa8);

    // OK <- in resolveUFCS with RemoveExp
    string[Variant9] aa9;
    aa9.remove(Variant9());

    // OK <- in DeleteExp::semantic
    Variant10* p10;
    delete p10;
}

void main() {}
