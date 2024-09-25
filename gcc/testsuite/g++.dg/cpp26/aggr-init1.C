// P3106R1 - Clarifying rules for brace elision in aggregate initialization
// Examples from C++26 [dcl.init.aggr]
// { dg-do run }

extern "C" void abort ();

namespace N1 {
#if __cpp_designated_initializers >= 201707L
  struct C {
    union {
      int a;
      const char* p;
    };
    int x;
  } c = { .a = 1, .x = 3 };
  constexpr C c2 = { .a = 1, .x = 3 };
  static_assert (c2.a == 1 && c2.x == 3, "");
#endif

  bool
  test ()
  {
#if __cpp_designated_initializers >= 201707L
    return c.a == 1 && c.x == 3;
#else
    return true;
#endif
  }
}

namespace N2 {
  struct A {
    int x;
    struct B {
      int i;
      int j;
    } b;
  } a = { 1, { 2, 3 } };

#if __cplusplus >= 201703L
  struct base1 { int b1, b2 = 42; };
  struct base2 {
    base2 () { b3 = 42; }
    int b3;
  };
  struct derived : base1, base2 {
    int d;
  };

  derived d1 { { 1, 2 }, {}, 4 };
  derived d2 { {}, {}, 4 };
#endif

  bool
  test ()
  {
    return a.x == 1 && a.b.i == 2 && a.b.j == 3
#if __cplusplus >= 201703L
	   && d1.b1 == 1 && d1.b2 == 2 && d1.b3 == 42 && d1.d == 4
	   && d2.b1 == 0 && d2.b2 == 42 && d2.b3 == 42 && d2.d == 4
#endif
	   ;
  }

#if __cplusplus >= 201703L
  constexpr A a2 = { 1, { 2, 3 } };
  static_assert (a2.x == 1 && a2.b.i == 2 && a2.b.j == 3, "");

  struct base3 {
#if __cplusplus >= 202002L
    constexpr base3 () { b3 = 42; }
#else
    constexpr base3 () : b3 (42) {}
#endif
    int b3;
  };
  struct derived2 : base1, base3 {
    int d;
  };
  constexpr derived2 d3 { { 1, 2}, {}, 4};
  constexpr derived2 d4 { {}, {}, 4 };
  static_assert (d3.b1 == 1 && d3.b2 == 2 && d3.b3 == 42 && d3.d == 4, "");
  static_assert (d4.b1 == 0 && d4.b2 == 42 && d4.b3 == 42 && d4.d == 4, "");
#endif
}

namespace N3 {
#if __cplusplus >= 201402L
  struct S { int a; const char *b; int c; int d = b[a]; };
  S ss = { 1, "asdf" };
  constexpr S ss2 = { 1, "asdf" };
  static_assert (ss2.a == 1 && ss2.b[0] == 'a' && ss2.b[3] == 'f' && ss2.c == 0 && ss2.d == 's', "");

#if __cpp_designated_initializers >= 201707L
  struct string { int s = -42; };
  struct A {
    string a;
    int b = 42;
    int c = -1;
  };
  static_assert (A { .c = 21 }.a.s == -42 && A { .c = 21 }.b == 42 && A { .c = 21 }.c == 21, "");
#endif
#endif

  bool
  test ()
  {
#if __cplusplus >= 201402L
    return ss.a == 1 && __builtin_strcmp (ss.b, "asdf") == 0 && ss.c == 0 && ss.d == 's';
#else
    return true;
#endif
  }
}

namespace N4 {
  int x[] = { 1, 3, 5 };

  bool
  test ()
  {
    return sizeof (x) == 3 * sizeof (int) && x[0] == 1 && x[1] == 3 && x[2] == 5;
  }

#if __cplusplus >= 201103L
  constexpr int x2[] = { 1, 3, 5 };
  static_assert (sizeof (x2) == 3 * sizeof (int)
		 && x2[0] == 1 && x2[1] == 3 && x2[2] == 5, "");
#endif
}

namespace N5 {
  struct X { int i, j, k; };
  X a[] = { 1, 2, 3, 4, 5, 6 };
  X b[2] = { { 1, 2, 3 }, { 4, 5, 6 } };

  bool
  test ()
  {
    return sizeof (a) == sizeof (b) && __builtin_memcmp (a, b, sizeof (a)) == 0;
  }

#if __cplusplus >= 201103L
  constexpr X a2[] = { 1, 2, 3, 4, 5, 6 };
  constexpr X b2[2] = { { 1, 2, 3 }, { 4, 5, 6 } };
  static_assert (sizeof (a2) == 2 * sizeof (X)
		 && a2[0].i == 1 && a2[0].j == 2 && a2[0].k == 3
		 && a2[1].i == 4 && a2[1].j == 5 && a2[1].k == 6, "");
  static_assert (sizeof (b2) == 2 * sizeof (X)
		 && b2[0].i == 1 && b2[0].j == 2 && b2[0].k == 3
		 && b2[1].i == 4 && b2[1].j == 5 && b2[1].k == 6, "");
#endif
}

namespace N7 {
  struct A {
    int i;
    static int s;
    int j;
    int : 17;
    int k;
  } a = { 1, 2, 3 };

  bool
  test ()
  {
    return a.i == 1 && a.j == 2 && a.k == 3;
  }

#if __cplusplus >= 201103L
  constexpr A a2 = { 1, 2, 3 };
  static_assert (a2.i == 1 && a2.j == 2 && a2.k == 3, "");
#endif
}

namespace N9 {
  int x[2][2] = { 3, 1, 4, 2 };
  float y[4][3] = {
    { 1 }, { 2 }, { 3 }, { 4 }
  };

  bool
  test ()
  {
    return x[0][0] == 3 && x[0][1] == 1 && x[1][0] == 4 && x[1][1] == 2
	   && y[0][0] == 1.f && y[0][1] == 0.f && y[0][2] == 0.f
	   && y[1][0] == 2.f && y[1][1] == 0.f && y[1][2] == 0.f
	   && y[2][0] == 3.f && y[2][1] == 0.f && y[2][2] == 0.f
           && y[3][0] == 4.f && y[3][1] == 0.f && y[3][2] == 0.f;
  }

#if __cplusplus >= 201103L
  constexpr int x2[2][2] = { 3, 1, 4, 2 };
  constexpr float y2[4][3] = {
    { 1 }, { 2 }, { 3 }, { 4 }
  };
  static_assert (x2[0][0] == 3 && x2[0][1] == 1 && x2[1][0] == 4 && x2[1][1] == 2, "");
  static_assert (y2[0][0] == 1.f && y2[0][1] == 0.f && y2[0][2] == 0.f, "");
  static_assert (y2[1][0] == 2.f && y2[1][1] == 0.f && y2[1][2] == 0.f, "");
  static_assert (y2[2][0] == 3.f && y2[2][1] == 0.f && y2[2][2] == 0.f, "");
  static_assert (y2[3][0] == 4.f && y2[3][1] == 0.f && y2[3][2] == 0.f, "");
#endif
}

namespace N10 {
  struct S1 { int a, b; };
  struct S2 { S1 s, t; };

  S2 x[2] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  S2 y[2] = {
    {
      { 1, 2 },
      { 3, 4 }
    },
    {
      { 5, 6 },
      { 7, 8 }
    }
  };

  bool
  test ()
  {
    return sizeof (x) == sizeof (y) && __builtin_memcmp (x, y, sizeof (x)) == 0;
  }

#if __cplusplus >= 201103L
  constexpr S2 x2[2] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  constexpr S2 y2[2] = { { { 1, 2 }, { 3, 4 } }, { { 5, 6 }, { 7, 8 } } };
  static_assert (x2[0].s.a == 1 && x2[0].s.b == 2 && x2[0].t.a == 3 && x2[0].t.b == 4, "");
  static_assert (x2[1].s.a == 5 && x2[1].s.b == 6 && x2[1].t.a == 7 && x2[1].t.b == 8, "");
  static_assert (y2[0].s.a == 1 && y2[0].s.b == 2 && y2[0].t.a == 3 && y2[0].t.b == 4, "");
  static_assert (y2[1].s.a == 5 && y2[1].s.b == 6 && y2[1].t.a == 7 && y2[1].t.b == 8, "");
#endif
}

namespace N12 {
  float y[4][3] = {
    { 1, 3, 5 },
    { 2, 4, 6 },
    { 3, 5, 7 },
  };
  float y2[4][3] = { 1, 3, 5, 2, 4, 6, 3, 5, 7 };

  bool
  test ()
  {
    for (int i = 0; i < 4; ++i)
      for (int j = 0; j < 3; ++j)
	if (y[i][j] != (i == 3 ? 0.f : (float) (i + 1 + j * 2))
	    || y[i][j] != y2[i][j])
	  return false;
    return true;
  }

#if __cplusplus >= 201103L
  constexpr float y3[4][3] = { { 1, 3, 5 }, { 2, 4, 6 }, { 3, 5, 7 }, };
  constexpr float y4[4][3] = { 1, 3, 5, 2, 4, 6, 3, 5, 7 };
  static_assert (y3[0][0] == 1.f && y3[0][1] == 3.f && y3[0][2] == 5.f, "");
  static_assert (y3[1][0] == 2.f && y3[1][1] == 4.f && y3[1][2] == 6.f, "");
  static_assert (y3[2][0] == 3.f && y3[2][1] == 5.f && y3[2][2] == 7.f, "");
  static_assert (y3[3][0] == 0.f && y3[3][1] == 0.f && y3[3][2] == 0.f, "");
  static_assert (y4[0][0] == 1.f && y4[0][1] == 3.f && y4[0][2] == 5.f, "");
  static_assert (y4[1][0] == 2.f && y4[1][1] == 4.f && y4[1][2] == 6.f, "");
  static_assert (y4[2][0] == 3.f && y4[2][1] == 5.f && y4[2][2] == 7.f, "");
  static_assert (y4[3][0] == 0.f && y4[3][1] == 0.f && y4[3][2] == 0.f, "");
#endif
}

namespace N13 {
  bool
  test ()
  {
    struct S { } s;
    struct A {
      S s1;
      int i1;
      S s2;
      int i2;
      S s3;
      int i3;
    } a = {
      { },
      0,
      s,
      0
    };
    return a.i1 == 0 && a.i2 == 0;
  }
}

namespace N14 {
  struct A {
    int i;
    operator int ();
  };
  struct B {
    A a1, a2;
    int z;
  };
  A a;
  B b = { 4, a, a };
  A::operator int () { return 42; }

  bool
  test ()
  {
    return b.a1.i == 4 && b.a2.i == 0 && b.z == 42;
  }

#if __cplusplus >= 201103L
  struct A2 {
    int i;
    constexpr operator int () const { return 42; }
  };
  struct B2 {
    A2 a1, a2;
    int z;
  };
  constexpr A2 a2 = { 26 };
  constexpr B2 b2 = { 4, a2, a2 };
  static_assert (b2.a1.i == 4 && b2.a2.i == 26 && b2.z == 42, "");
#endif
}

int
main ()
{
  if (!N1::test ()
      || !N2::test ()
      || !N3::test ()
      || !N4::test ()
      || !N5::test ()
      || !N7::test ()
      || !N9::test ()
      || !N10::test ()
      || !N12::test ()
      || !N13::test ()
      || !N14::test ())
    abort ();
}
