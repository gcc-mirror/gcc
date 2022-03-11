// { dg-do compile }
// { dg-options "-fopenmp -foffload-memory=unified -fdump-tree-usm_transform" }

struct t1
{
  int a;
  int b;
};

typedef unsigned char uint8_t;

void
foo (__SIZE_TYPE__ x, __SIZE_TYPE__ y)
{
  uint8_t *p1 = new uint8_t;
  uint8_t *p2 = new uint8_t[20];
  t1 *p3 = new t1;
  t1 *p4 = new t1[y];
  delete p1;
  delete p3;
  delete [] p2;
  delete [] p4;
}

/* { dg-final { scan-tree-dump-times "omp_alloc \\(1, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_alloc \\(20, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_alloc" 4 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_free" 4 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not "operator new"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not "operator delete"  "usm_transform"  } } */
