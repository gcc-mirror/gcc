// Contributed by Matt Austern <austern@apple.com>

void f ()
{
  int n;
  (char) n = 1; // { dg-error "" }
}
