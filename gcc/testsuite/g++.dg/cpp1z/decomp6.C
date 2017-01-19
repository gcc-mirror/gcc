// { dg-do run { target c++11 } }
// { dg-options "" }

int ccnt, dcnt, cccnt, tccnt;

struct A
{
  A () : a (6) { ccnt++; }
  ~A () { dcnt++; }
  explicit A (const A &x) : a (x.a) { cccnt++; }
  template <typename T>
  A (const T &x) : a (x.a) { tccnt++; }
  int a;
};

int
main ()
{
  if (ccnt || dcnt || cccnt || tccnt)
    __builtin_abort ();
  {
    A a[6];
    if (ccnt != 6 || dcnt || cccnt || tccnt)
      __builtin_abort ();
    {
      auto [b,c,d,e,f,g] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
      if (ccnt != 6 || dcnt || cccnt || tccnt != 6)
	__builtin_abort ();
      b.a++;
      c.a += 2;
      f.a += 3;
      if (b.a != 7 || c.a != 8 || d.a != 6 || e.a != 6 || f.a != 9 || g.a != 6)
	__builtin_abort ();
      if (&b == &a[0] || &c == &a[1] || &d == &a[2] || &e == &a[3] || &f == &a[4] || &g == &a[5])
	__builtin_abort ();
      {
	auto&[ h, i, j, k, l, m ] = a;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
	if (ccnt != 6 || dcnt || cccnt || tccnt != 6)
	  __builtin_abort ();
	j.a += 4;
	k.a += 5;
	m.a += 6;
	if (a[0].a != 6 || a[1].a != 6 || a[2].a != 10 || a[3].a != 11 || a[4].a != 6 || a[5].a != 12)
	  __builtin_abort ();
	if (&h != &a[0] || &i != &a[1] || &j != &a[2] || &k != &a[3] || &l != &a[4] || &m != &a[5])
	  __builtin_abort ();
      }
      if (ccnt != 6 || dcnt || cccnt || tccnt != 6)
	__builtin_abort ();
    }
    if (ccnt != 6 || dcnt != 6 || cccnt || tccnt != 6)
      __builtin_abort ();
  }
  if (ccnt != 6 || dcnt != 12 || cccnt || tccnt != 6)
    __builtin_abort ();

  {
    A a[6];
    if (ccnt != 12 || dcnt != 12 || cccnt || tccnt != 6)
      __builtin_abort ();
    {
      auto [b,c,d,e,f,g] { a };		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
      if (ccnt != 12 || dcnt != 12 || cccnt != 6 || tccnt != 6)
	__builtin_abort ();
      b.a++;
      c.a += 2;
      f.a += 3;
      if (b.a != 7 || c.a != 8 || d.a != 6 || e.a != 6 || f.a != 9 || g.a != 6)
	__builtin_abort ();
      if (&b == &a[0] || &c == &a[1] || &d == &a[2] || &e == &a[3] || &f == &a[4] || &g == &a[5])
	__builtin_abort ();
      {
	auto&[ h, i, j, k, l, m ] {a};	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
	if (ccnt != 12 || dcnt != 12 || cccnt != 6 || tccnt != 6)
	  __builtin_abort ();
	j.a += 4;
	k.a += 5;
	m.a += 6;
	if (a[0].a != 6 || a[1].a != 6 || a[2].a != 10 || a[3].a != 11 || a[4].a != 6 || a[5].a != 12)
	  __builtin_abort ();
	if (&h != &a[0] || &i != &a[1] || &j != &a[2] || &k != &a[3] || &l != &a[4] || &m != &a[5])
	  __builtin_abort ();
      }
      if (ccnt != 12 || dcnt != 12 || cccnt != 6 || tccnt != 6)
	__builtin_abort ();
    }
    if (ccnt != 12 || dcnt != 18 || cccnt != 6 || tccnt != 6)
      __builtin_abort ();
  }
  if (ccnt != 12 || dcnt != 24 || cccnt != 6 || tccnt != 6)
    __builtin_abort ();

  {
    A a[6];
    if (ccnt != 18 || dcnt != 24 || cccnt != 6 || tccnt != 6)
      __builtin_abort ();
    {
      auto [b,c,d,e,f,g] ( a );		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
      if (ccnt != 18 || dcnt != 24 || cccnt != 12 || tccnt != 6)
	__builtin_abort ();
      b.a++;
      c.a += 2;
      f.a += 3;
      if (b.a != 7 || c.a != 8 || d.a != 6 || e.a != 6 || f.a != 9 || g.a != 6)
	__builtin_abort ();
      if (&b == &a[0] || &c == &a[1] || &d == &a[2] || &e == &a[3] || &f == &a[4] || &g == &a[5])
	__builtin_abort ();
      {
	auto&[ h, i, j, k, l, m ] (a);	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
	if (ccnt != 18 || dcnt != 24 || cccnt != 12 || tccnt != 6)
	  __builtin_abort ();
	j.a += 4;
	k.a += 5;
	m.a += 6;
	if (a[0].a != 6 || a[1].a != 6 || a[2].a != 10 || a[3].a != 11 || a[4].a != 6 || a[5].a != 12)
	  __builtin_abort ();
	if (&h != &a[0] || &i != &a[1] || &j != &a[2] || &k != &a[3] || &l != &a[4] || &m != &a[5])
	  __builtin_abort ();
      }
      if (ccnt != 18 || dcnt != 24 || cccnt != 12 || tccnt != 6)
	__builtin_abort ();
    }
    if (ccnt != 18 || dcnt != 30 || cccnt != 12 || tccnt != 6)
      __builtin_abort ();
  }
  if (ccnt != 18 || dcnt != 36 || cccnt != 12 || tccnt != 6)
    __builtin_abort ();
}
