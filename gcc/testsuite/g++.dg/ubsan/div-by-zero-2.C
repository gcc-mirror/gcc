// PR sanitizer/80348
// { dg-do compile }
// { dg-options "-fsanitize=integer-divide-by-zero" }

void
foo ()
{
  if (0)
    unsigned ((0 != 60806) > (0 != 0)) / 0; // { dg-warning "division by zero" }
}
