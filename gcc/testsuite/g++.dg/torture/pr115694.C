// { dg-do compile }

_Complex a;
typedef struct {
  double a[2];
} b;
void c(b);
void d()
{
  _Complex b1 = a;
  b t = __builtin_bit_cast (b, b1);
  c(t);
}
