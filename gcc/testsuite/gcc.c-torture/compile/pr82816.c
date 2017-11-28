struct A
{
  int b:3;
} d, e;

int c;

void f ()
{
  char g = d.b * e.b;
  c = g;
}
