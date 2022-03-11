/* { dg-do compile } */
/* { dg-additional-options "-foffload-memory=unified -fdump-tree-usm_transform" } */

#ifdef __cplusplus
extern "C" {
#endif

void *malloc (__SIZE_TYPE__);
void *aligned_alloc (__SIZE_TYPE__, __SIZE_TYPE__);
void *calloc(__SIZE_TYPE__, __SIZE_TYPE__);
void *realloc(void *, __SIZE_TYPE__);
void free (void *);

#ifdef __cplusplus
}
#endif

void
foo ()
{
  void *p1 = malloc(20);
  void *p2 = realloc(p1, 30);
  void *p3 = calloc(4, 15);
  void *p4 = aligned_alloc(16, 40);
  free (p2);
  free (p3);
  free (p4);
}

/* { dg-final { scan-tree-dump-times "omp_alloc \\(20, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_realloc \\(.*, 30, 10, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_calloc \\(4, 15, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_aligned_alloc \\(16, 40, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_free" 3 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " free"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " aligned_alloc"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " malloc"  "usm_transform"  } } */
