/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */
/* { dg-require-effective-target int32plus } */

struct
{
  int f0:24;
} a, c, d;

int b;

int
fn1 ()
{
  return 0;
}

void
fn2 ()
{
  int e;
  if (b) 
    for (; e;)
      {
	d = c;
	if (fn1 (b))
	  b = a.f0;
      }
}
