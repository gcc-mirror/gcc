/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

template<typename T, typename T2>
void f (T x, T2 y) 
{
  #pragma omp metadirective when(user={condition(x)}, \
				 target_device={device_num(y)} : flush)
}

class c
{
 public:
  int x;
  c (int xx) { x = xx; }
  operator bool() { return x != 0; }
};

template <typename T> class d
{
 public:
  T x;
  d (T xx) { x = xx; }
  operator bool() { return x != 0; }
};

int main (void)
{
  c obj1 (42);
  d<int> obj2 (69);

  f (42, 0);
  f (&obj1, 0);
  f (obj1, 0);
  f (obj2, 0);
}

/* { dg-final { scan-tree-dump-times "if \\(x != 0 &&" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "if \\(x != 0B &&" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "if \\(<<cleanup_point c::operator bool \\(&x\\)>> &&" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "if \\(<<cleanup_point d<int>::operator bool \\(&x\\)>> &&" 1 "original" } } */
