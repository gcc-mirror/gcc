// PR debug/94323
// { dg-do compile }
// { dg-options "-O2 -fcompare-debug" }
// { dg-xfail-if "AIX compare debug" { powerpc-ibm-aix* } }

volatile int a;

void
foo ()
{
  ({
     a;
   });
}
