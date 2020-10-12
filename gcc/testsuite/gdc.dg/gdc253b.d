// https://bugzilla.gdcproject.org/show_bug.cgi?id=253
// { dg-additional-sources "imports/gdc253b.d" }
// { dg-options "-I $srcdir/gdc.dg" }
// { dg-do compile }

import imports.gdc253b;

interface A253
{
    void test253(int[int]);
}

interface C253 : A253
{
}

class D253 : B253, C253
{
}
