// { dg-do assemble  }
// GROUPS passed templates

template<class T>
class Vector
{
  int sz;
  T *v;
public:
  Vector (int s) : sz (s) { v = new T[sz]; }
  ~Vector () { delete[] v; }
  T &operator[] (int i) { return v[i]; }
  int size () { return sz; }
};

template<class T>// { dg-error "" } previous definition of T
struct Comparator
{
  typedef T T;// { dg-error "" } use of template type T in typedef to T
  static int lessthan (T &a, T &b) { return a < b; }
};

template<class Comp>
struct Sort
{
  static void sort (Vector<Comp::T> &);// { dg-error "" } use of bad T
};

template<class Comp>
void Sort<Comp>::sort (Vector<Comp::T> &v)// { dg-error "" } use of bad T
{
  int n = v.size ();

  for (int i = 0; i < n - 1; i++)
    for (int j = n - 1; i < j; j--)
      if (Comp::lessthan (v[j], v[j - 1]))
	{
	  typename Comp::T temp = v[j];
	  v[j] = v[j - 1];
	  v[j - 1] = temp;
	}
}

void
f (Vector<int> &vi)
{
  Sort<Comparator<int> >::sort (vi);
}
