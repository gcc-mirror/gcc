/* PR middle-end/101600 - Spurious -Warray-bounds downcasting a polymorphic
   pointer
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct S1 { virtual ~S1(); };
struct S2 { int m; };
struct S3 { virtual ~S3(); };
struct S4: S1, S2, S3 {};

int f1 ();

void f2 (S3 *);

void f3 (S2 *p)
{
  for (int i = f1 (); f1 (); )
    {
      if (i == 0)
  	{
  	  p = 0;
  	  break;
  	}
    }

  f2 (static_cast<S4 *>(p));  // { dg-bogus "-Warray-bounds" }
}
