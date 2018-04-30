// PR c++/84704
// { dg-do compile }
// { dg-options "-g -fcompare-debug -O2" }
// { dg-xfail-if "" { powerpc-ibm-aix* } }

int a[1] = { 0 };

void
foo ()
{
  a[({ 0; })] %= 5;
}
