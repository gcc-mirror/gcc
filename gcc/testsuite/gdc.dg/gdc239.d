// https://bugzilla.gdcproject.org/show_bug.cgi?id=239
// { dg-options "-I $srcdir/gdc.dg" }
// { dg-do compile }

import imports.gdc239;

class C239
{
    C239a *foo;
}
