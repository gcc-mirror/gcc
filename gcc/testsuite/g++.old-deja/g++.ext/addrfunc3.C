// { dg-do run  }
// { dg-options "-fpermissive -w" }
// Test for overload resolution in comparison expressions.
// Contributed by Jason Merrill <jason@cygnus.com>.

void f (int) { }
void f ();

int main () {
  void (*p)(int);
  p = f;
  if (p != f)
    return 1;
}
