// PR c++/35334
// { dg-do compile }
// { dg-bogus "not supported by" "" { target *-*-* } 0 }

__complex__ unsigned int i;
int j;
char k;
__complex__ double l;
double m;
float n;

void
foo ()
{
  ((__complex__ int)i)();		// { dg-error "cannot be used as a function" }
  ((__complex__ int)j)();		// { dg-error "cannot be used as a function" }
  ((__complex__ int)k)();		// { dg-error "cannot be used as a function" }
  ((__complex__ long double)l)();	// { dg-error "cannot be used as a function" }
  ((__complex__ long double)m)();	// { dg-error "cannot be used as a function" }
  ((__complex__ long double)n)();	// { dg-error "cannot be used as a function" }
}
