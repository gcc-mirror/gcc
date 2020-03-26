// PR debug/94323
// { dg-do compile }
// { dg-options "-O2 -fcompare-debug" }

volatile int a;

void
foo ()
{
  ({
     a;
   });
}
