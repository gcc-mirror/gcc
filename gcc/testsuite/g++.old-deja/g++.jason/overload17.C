// Bug: g++ fails to prefer UDC's alone to UDC's plus standard conversions.
// Build don't link:

struct B { };
struct D: public B { };
struct DP {
  operator D * () const;
  operator double () const;
};

void f (B *);
void f (D *);
void g (double);
void g (float);

void h (DP p)
{
  f (p);
  g (p);
}
