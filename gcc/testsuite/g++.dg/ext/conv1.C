// Test for backwards brain-damage compatibility with -fpermissive.
// { dg-options "-fpermissive -w" }

void f ();
void f (int *);
void g (int);

int main ()
{
  void *v = 1234;
  void (*p)() = v;
  int i = v;
  f (i);
  f (v);
  g (v);
}
