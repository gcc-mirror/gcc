// { dg-do compile { target c++17 } }
// { dg-options "-fopenmp -fdump-tree-usm_transform" }

#pragma omp requires unified_shared_memory

struct t1
{
  int a;
  int b;
};

typedef unsigned char uint8_t;

void
foo (__SIZE_TYPE__ x, __SIZE_TYPE__ y)
{
  uint8_t *p1 = new (std::align_val_t(128)) uint8_t;
  uint8_t *p2 = new (std::align_val_t(128)) uint8_t[40];
  t1 *p3 = new (std::align_val_t(128)) t1;
  t1 *p4 = new (std::align_val_t(128)) t1[y];
  delete p1;
  delete p3;
  delete [] p2;
  delete [] p4;
}

/* { dg-final { scan-tree-dump-times "omp_aligned_alloc \\(128, 1, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_aligned_alloc \\(128, 40, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_aligned_alloc" 4 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_free" 4 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not "operator new"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not "operator delete"  "usm_transform"  } } */
