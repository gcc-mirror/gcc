// PR c++/31489

class foo;   // { dg-error "'class foo'" }
struct bar;  // { dg-error "'struct bar'" }

int main()
{
  foo* f = new foo; // { dg-error "'class foo'" }
  bar* b = new bar; // { dg-error "'struct bar'" }
}
