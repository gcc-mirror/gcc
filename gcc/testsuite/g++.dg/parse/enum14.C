// PR c++/96077

int main ()
{
  enum { E = (2 } e; // { dg-error "expected" }
  enum { F = true ? 2 : (3 /* missing ")" here */ } f; // { dg-error "expected" }
}
