// Submitted by Jason Merrill <jason_merrill@redhat.com>
// Test for proper handling of local static references.
// { dg-do run }

int r;

int c;
int f ()
{
  // Test that we only initialize i once.
  if (++c > 1)
    ++r;
  return 42;
}

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

int main ()
{
  g ();
  h ();
  return r;
}
