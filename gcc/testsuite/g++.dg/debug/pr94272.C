// PR c++/94272
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fcompare-debug" }

int *c, d, *e;

void
foo ()
{
  if (c && d)
    ;
  else if (*e)
    ;
}
