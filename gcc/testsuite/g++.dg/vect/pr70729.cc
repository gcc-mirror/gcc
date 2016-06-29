// { dg-do compile }
// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-Ofast" }
// { dg-additional-options "-mavx2 -fopenmp-simd" { target x86_64-*-* i?86-*-* } }


#include <string.h>
#include <xmmintrin.h>

inline void* my_alloc(size_t bytes) {return _mm_malloc(bytes, 128);}
inline void my_free(void* memory) {_mm_free(memory);}

template <typename T>
class Vec
{
  const int isize;
	T* data;

public:

  Vec (int n) : isize(n) {data = (T*)my_alloc(isize*sizeof(T));}
  ~Vec () {my_free(data);}

  Vec& operator = (const Vec& other)	
    {
      if (this != &other)
	memcpy(data, other.data, isize*sizeof(T));
      return *this;
    }

  T& operator [] (int i) {return data[i];}
  const T& operator [] (int i) const {return data[i];}
  T& at (int i)  {return data[i];}
  const T& at (int i) const {return data[i];}

  operator T* ()  {return data;}
  int size () const {return isize;}
};

template <typename T>                                  
class Cl
{
public:

  Cl (int n, int m);
  const int N, M;
  Vec<T> v_x, v_y;
  Vec<int> v_i;
  Vec<float> v_z;
};

struct Ss
{
    const int S_n, S_m;
    Cl<float> v1;
    float* C1;
    float* C2;
    Ss (int n1, int n2): S_n(n1), S_m(n2), v1(n1, n2)
      {
	C1 = new float[n1 * 3];
	C2 = new float[n2 * 4]; 
      }

    ~Ss () { delete C1; delete C2;}
   void foo (float *in, float w);
};
void Ss::foo (float *in, float w)
{
#pragma omp simd
  for (int i=0; i<S_n; i++)
    {
      float w1 = C2[S_n + i] * w;
      v1.v_i[i] += (int)w1;
      C1[S_n + i] += w1;
    }
}
 
// { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } }
