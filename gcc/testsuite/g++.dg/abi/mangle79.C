// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  static void foo (S);
  void foo (this S);		// { dg-warning "explicit object member function only available with" "" { target c++20_down } }
  template <int N, typename T>
  static void bar (S, T);
  template <int N, typename T>
  void bar (this S, T);		// { dg-warning "explicit object member function only available with" "" { target c++20_down } }
  static void baz (const S &);
  void baz (this const S &);	// { dg-warning "explicit object member function only available with" "" { target c++20_down } }
};

void
S::foo (S)
{
}

void
S::foo (this S)			// { dg-warning "explicit object member function only available with" "" { target c++20_down } }
{
}

template <int N, typename T>
void
S::bar (S, T)
{
}

template <int N, typename T>
void
S::bar (this S, T)		// { dg-warning "explicit object member function only available with" "" { target c++20_down } }
{
}

void
S::baz (const S &)
{
}

void
S::baz (this const S &)		// { dg-warning "explicit object member function only available with" "" { target c++20_down } }
{
}

void
qux (S *p)
{
  S::foo (*p);
  p->foo ();
  S::bar <5> (*p, 0);
  p->bar <5> (0);
}

// { dg-final { scan-assembler "_ZN1S3fooES_" } }
// { dg-final { scan-assembler "_ZNH1S3fooES_" } }
// { dg-final { scan-assembler "_ZN1S3barILi5EiEEvS_T0_" } }
// { dg-final { scan-assembler "_ZNH1S3barILi5EiEEvS_T0_" } }
// { dg-final { scan-assembler "_ZN1S3bazERKS_" } }
// { dg-final { scan-assembler "_ZNH1S3bazERKS_" } }
