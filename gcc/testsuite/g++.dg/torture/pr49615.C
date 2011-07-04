/* { dg-do compile } */
/* { dg-options "-g" } */

template <class T>
static inline bool Dispatch (T* obj, void (T::*func) ())
{
  (obj->*func) ();
}
class C
{
  bool f (int);
  void g ();
};
bool C::f (int n)
{
  bool b;
  switch (n)
    {
      case 0:
	  b = Dispatch (this, &C::g);
      case 1:
	  b = Dispatch (this, &C::g);
    }
}
void C::g ()
{
  for (;;) { }
}

