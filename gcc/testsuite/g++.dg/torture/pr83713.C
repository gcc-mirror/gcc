// { dg-do compile }

class a
{
  char b;
  void c ();
};
void
a::c ()
{
  &b + ((long long) &b & 0);
}
