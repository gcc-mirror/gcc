// Build don't link: 

extern void byebye ();
template <class T1, class T2>
struct A
{
  T1 t1;
  T2 t2;
  A() { t1 = 0; t2 = 0; }
  ~A() { byebye(); }
};

template <class Q>
int f (A<int, Q> a) {
  return a.t1;
}

extern A<int,double*> aa;
int foop () { return f(aa); }
