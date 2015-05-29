// { dg-do compile }
// { dg-options "-O2 -fstrict-aliasing -fdump-tree-pre-details" }

// Make sure we hoist invariants out of the loop even in the presence
// of placement new.  This is similar to code in tramp3d.

typedef __SIZE_TYPE__ size_t;

inline void* operator new(size_t, void* __p) throw() { return __p; }

template <class T, int D>
class Vector
{
public:
   Vector()
   {
     for (int i = 0; i < D; ++i)
        new (&x_m[i]) T();
   }
   T& operator[](int i) { return x_m[i]; }

private:
   T x_m[D];
};

struct sia
{
  int ai[3];
};

struct s
{
  struct si
  {
    sia* p;
  } asi[3];
  float* pd;
};

class c
{
  int foo(int, int, int);
  s sm;
};


extern void bar(Vector<float, 3>*, int);
int c::foo(int f1, int f2, int f3)
{
  float sum = 0;
  for (int i = 0; i < 3; ++i)
    {
      for (int j = 0; j < 3; ++j)
	{
	  Vector<float, 3> v;
	  v[0] = 1.0;
	  v[1] = 2.0;
	  v[2] = 3.0;
	  for (int k = 0; k < 3; ++k)
	    {
	      float f = (f1 * this->sm.asi[0].p->ai[0]
			 + f2 * this->sm.asi[1].p->ai[0]
			 + f3 * this->sm.asi[2].p->ai[0]);
	      sum += f * v[k];
	    }
	  *this->sm.pd = sum;
	}
    }
  return sum;
}

// { dg-final { scan-tree-dump "Replaced.*->ai\\\[0\\\]" "pre" } }
