// PR c++/94272
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fcompare-debug" }
// { dg-xfail-if "AIX compare debug" { powerpc-ibm-aix* } }

int *c, d, *e;

void
foo ()
{
  if (c && d)
    ;
  else if (*e)
    ;
}
