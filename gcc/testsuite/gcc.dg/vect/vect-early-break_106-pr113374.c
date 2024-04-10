/* { dg-do compile } */
/* { dg-add-options vect_early_break } */

typedef __SIZE_TYPE__ size_t;
struct S { unsigned char *a, *b; };

void
foo (struct S x)
{
  for (size_t i = x.b - x.a; i > 0; --i)
    {
      size_t t = x.b - x.a;
      size_t u = i - 1;
      if (u >= t)
        __builtin_abort ();
      if (x.a[i - 1]--)
        break;
    }
}
