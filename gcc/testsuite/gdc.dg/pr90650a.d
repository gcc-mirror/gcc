// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90650
// { dg-do compile }
class c
{
  static f ()
  {
    return 0;
  }
}

void g ()
{
  if (0 & [0] & c.f()) {}   // { dg-error "array operation .\\\[0\\\] & 0 & f\\(\\). without destination memory not allowed" }
}
