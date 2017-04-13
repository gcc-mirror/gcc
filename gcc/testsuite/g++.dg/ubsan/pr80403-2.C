// PR sanitizer/80403
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

extern const long long int v;
extern unsigned long int w;

int
foo ()
{
  int a = (0 - 40U <= (0 == 8)) << !w << (0 < v) == 0;
  int b = ((0 ^ 0) < (long) (1066066618772207110 <= 0)) / 0 << 0;	// { dg-warning "division by zero" }
  return a + b;
}
