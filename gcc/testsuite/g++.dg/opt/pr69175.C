// PR target/69175
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-march=armv7-a -mfloat-abi=hard -mfpu=vfpv3-d16 -mthumb" { target { arm_hard_vfp_ok && arm_thumb2_ok } } }

struct A { A *c, *d; } a;
struct B { A *e; A *f; void foo (); };
void *b;

void
B::foo ()
{
  if (b) 
    {
      A *n = (A *) b;
      if (b == e)
	if (n == f)
	  e = __null;
	else
	  e->c = __null;
      else
	n->d->c = &a;
      n->d = e;
      if (e == __null)
	e = f = n;
      else
	e = n;
    }
}
