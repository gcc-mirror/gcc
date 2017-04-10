// PR sanitizer/80348
// { dg-do compile }
// { dg-options "-fsanitize=integer-divide-by-zero" }

extern long long int i;
void
fn1 ()
{
  (0 >= 10253361740180 >= long (0 >= 0)) % i;
}

void
fn2 ()
{
  0 / unsigned (!(0 - 3) >= (0 > 0));
}

void
fn3 ()
{
  (0 < 0 >= (0 < 0 < 0)) % (unsigned (2) << 0);
}
