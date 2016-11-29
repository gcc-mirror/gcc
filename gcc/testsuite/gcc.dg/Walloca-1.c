/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=2000 -O2" } */

#define alloca __builtin_alloca

typedef __SIZE_TYPE__ size_t;
extern size_t strlen(const char *);

extern void useit (char *);

int num;

void foo1 (size_t len, size_t len2, size_t len3)
{
  int i;

  for (i=0; i < 123; ++i)
    {
      char *s = alloca (566);	/* { dg-warning "'alloca' within a loop" } */
      useit (s);
    }

  char *s = alloca (123);
  useit (s);			// OK, constant argument to alloca

  s = alloca (num);		// { dg-warning "large due to conversion" "" { target lp64 } }
  // { dg-warning "unbounded use of 'alloca'" "" { target { ! lp64 } } 26 }
  useit (s);

  s = alloca(90000);		/* { dg-warning "is too large" } */
  useit (s);

  if (len < 2000)
    {
      s = alloca(len);		// OK, bounded
      useit (s);
    }

  if (len + len2 < 2000)	// OK, bounded
    {
      s = alloca(len + len2);
      useit (s);
    }

  if (len3 <= 2001)
    {
      s = alloca(len3);		/* { dg-warning "may be too large" } */
      useit(s);
    }
}

void foo2 (__SIZE_TYPE__ len)
{
  // Test that a direct call to __builtin_alloca_with_align is not confused
  // with a VLA.
  void *p = __builtin_alloca_with_align (len, 8); // { dg-warning "unbounded use of 'alloca'" }
  useit (p);
}

void foo3 (unsigned char a)
{
  if (a == 0)
    useit (__builtin_alloca (a)); // { dg-warning "argument to 'alloca' is zero" }
}
