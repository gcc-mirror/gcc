// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-usm_transform" }

#pragma omp requires unified_shared_memory

#include <new>


struct X {
    static void* operator new(std::size_t count)
    {
      static char buf[10];
      return &buf[0];
    }
    static void* operator new[](std::size_t count)
    {
      static char buf[10];
      return &buf[0];
    }
    static void operator delete(void*)
    {
    }
    static void operator delete[](void*)
    {
    }
};
void foo() {
  X* p1 = new X;
  delete p1;
  X* p2 = new X[10];
  delete[] p2;
  unsigned char buf[24] ;
  int *p3 = new (buf) int(3);
  p3[0] = 1;
}

/* { dg-final { scan-tree-dump-not "omp_alloc"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not "omp_free"  "usm_transform"  } } */
