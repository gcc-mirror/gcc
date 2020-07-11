typedef struct { int *a; } S;
int *f (void);
static void g (S *x)
{
  int *p = x->a;
  p[0] = 0;
}
void h (void)
{
  S x[1];
  x->a = f ();
  g (x);
}
