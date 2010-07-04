// PR c++/37561
// { dg-do compile }
// { dg-options "-Wno-int-to-pointer-cast" }

__PTRDIFF_TYPE__ p;
char q;

void
foo ()
{
  ((char *) p)++;	// { dg-error "lvalue" }
  ((char *) q)++;	// { dg-error "lvalue" }
  ((char *) p)--;	// { dg-error "lvalue" }
  ((char *) q)--;	// { dg-error "lvalue" }
  ++(char *) p;		// { dg-error "lvalue" }
  ++(char *) q;		// { dg-error "lvalue" }
  --(char *) p;		// { dg-error "lvalue" }
  --(char *) q;		// { dg-error "lvalue" }
}
