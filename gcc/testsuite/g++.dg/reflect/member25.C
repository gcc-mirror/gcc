// PR c++/126794
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

struct A { int m{}; };
template<int> struct E : A {};

int
f1 ()
{
  E<2> *o = 0;
  return o->[:^^A::m:];
}

template<class W>
int
get (W *o)
{
  return o->[:^^A::m:];
}

int
f2 ()
{
  return get ((E<2> *) 0);
}

template<class W>
void
f3 (W *o)
{
  same_type<decltype (o->[:^^A::m:]), int>();
  same_type<decltype ((o->[:^^A::m:])), int &>();
}
template void f3 (E<3> *);

template<int> struct F;

template<class W>
int
f4 (W *o)
{
  return o->[:^^A::m:];   // { dg-error "invalid use of incomplete type" }
}

int
f5 ()
{
  return f4 ((F<0> *) 0);
}

template<class W>
int
f6 (W &o)
{
  return o.[:^^A::m:];
}
template int f6 (E<4> &);
