// Test for overload resolution in comparison expressions.
// Contributed by Jason Merrill <jason@cygnus.com>.
// Special g++ Options: -fpermissive -w

void f (int) { }
void f ();

int main () {
  void (*p)(int);
  p = f;
  if (p != f)
    return 1;
}
