extern void u (int, int);
extern void v (float, float);

void f (__complex__ int x)
{
  u (0, x);
}

void g (__complex__ float x)
{
  v (0, x);
}
