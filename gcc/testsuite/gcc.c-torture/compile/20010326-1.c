float d;
int e, f;

void foo (void)
{
  struct { float u, v; } a = {0.0, 0.0};
  float b;
  int c;

  c = e;
  if (c == 0)
    c = f;
  b = d;
  if (a.v < b)
    a.v = b;
}
