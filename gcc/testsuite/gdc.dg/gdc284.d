// https://bugzilla.gdcproject.org/show_bug.cgi?id=284
// { dg-options "-Wno-psabi" }
// { dg-do compile }

alias v284 = __vector(int[2]);

v284 test284(v284 a, ...)
{
    return a + a;
}
