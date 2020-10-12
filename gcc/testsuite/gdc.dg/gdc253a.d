// https://bugzilla.gdcproject.org/show_bug.cgi?id=253
// { dg-additional-sources "imports/gdc253a.d" }
// { dg-do compile }

import imports.gdc253a;

class C253 : C253a
{
    void test253() { }
}
