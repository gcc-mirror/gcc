// PR sanitizer/80403
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

unsigned
foo ()
{
  unsigned a = (unsigned) (!(6044238 >> 0) >= (0 < 0)) % 0;	// { dg-warning "division by zero" }
  unsigned b = (unsigned) (!(6044238 >> 0) >= (0 < 0)) / 0;	// { dg-warning "division by zero" }
  return a + b;
}
