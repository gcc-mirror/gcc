// DR2351
// { dg-do compile { target c++11 } }

void
foo ()
{
  void{};
  void();
}

template <class ...T>
void
bar (T... t)
{
  void{t...};
  void(t...);
}

void
baz ()
{
  bar ();
}

template <class ...T>
void
qux (T... t)
{
  void{t...};	// { dg-error "compound literal of non-object type" }
}

void
corge ()
{
  qux (1, 2);
}

template <class ...T>
void
garply (T... t)
{
  void{t..., t..., t...};
  void(t..., t..., t...);
}

template <class ...T>
void
grault (T... t)
{
  void{t..., 1};	// { dg-error "compound literal of non-object type" }
}
