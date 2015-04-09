// PR ipa/65610
// { dg-do compile }
// { dg-options "-std=c++11 -fsanitize=undefined -fno-sanitize=vptr -fcompare-debug" }

class A;
class B {};
enum C { D };
class E;
class F;
class G;
class H
{
  F m1 (const A &t) const;
  G m2 () const;
};
class G {};
template <class S, class T>
class I;
template <class S, class T>
class J
{
  friend class I <S,T>;
  J <S,T> *j;
};
template <class S, class T>
struct I
{
  virtual ~I ();
  virtual void m3 (void *p) {}
  J <S,T> *i;
  void m4 (J <S,T>*& t);
};
template <class S, class T>
void I <S,T>::m4 (J <S,T> * &t)
{
  m4 (t->j);
  m3 (t);
}
template <class S, class T>
I <S,T>::~I ()
{
  m4 (i);
}
struct F
{
  explicit inline F (C v);
  inline ~F ();
  I <B, E> f;
};
inline F::F (C v) {}
inline F::~F () {}
F H::m1 (const A &t) const
{
  F q (D);
  G r = m2 ();
  return q;
}
