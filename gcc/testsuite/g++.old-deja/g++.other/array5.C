// Build don't link:
// We tried to create a temporary of unknown size and crashed.

extern int a1[];
extern int a2[];
int foo(int p)
{
  int x = (p ? a1 : a2)[1];
  return x;
}
