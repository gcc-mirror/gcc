// PR c++/44444
// { dg-do compile }
// { dg-options "-Wunused" }

struct S
{
  const int &u;
  const int &v;
  S (const int &a, const int &b) : u(a), v(b) { }
};

bool
f1 ()
{
  bool t = false;
  S z = S (1, 2);
  t |= z.u == 1;
  t |= z.v == 2;
  return t;
}

void
f2 ()
{
  S z = S (1, 2);
  z.u;		// { dg-warning "no effect" }
}

int i;

void
f3 ()
{
  S z = S (1, 2);
  i++, z.u;	// { dg-warning "no effect" }
}
