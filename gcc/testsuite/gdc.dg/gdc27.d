// { dg-options "-I $srcdir/gdc.dg/imports" }
// { dg-additional-sources "imports/gdc27.d" }
// { dg-do compile }

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=27

import imports.gdc27;

interface I_B : I_A
{
    void b();
}

abstract class C_B : C_A, I_B
{
    abstract void b();
}
