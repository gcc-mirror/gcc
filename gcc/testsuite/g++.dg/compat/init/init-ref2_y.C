extern int f (void);
extern int r;

const int *p;

void g ()
{
  static const int &i = f();

  // Test that i points to the same place in both calls.
  if (p && p != &i)
    ++r;
  // Test that if so, it points to static data.
  if (i != 42)
    ++r;

  p = &i;
}

void h ()
{
  int arr[] = { 1, 1, 1, 1, 1, 1, 1 };
  g ();
}
