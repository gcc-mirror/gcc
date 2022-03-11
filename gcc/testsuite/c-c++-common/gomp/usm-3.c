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
void *omp_target_alloc (__SIZE_TYPE__, int);
void omp_target_free (void *, int);

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
  void *p5 = omp_target_alloc(50, 1);
  free (p2);
  free (p3);
  free (p4);
  omp_target_free (p5, 1);
}

/* { dg-final { scan-tree-dump-times "omp_alloc \\(20, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_realloc \\(.*, 30, 10, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_calloc \\(4, 15, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_aligned_alloc \\(16, 40, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_alloc \\(50, 10\\)" 1 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-times "omp_free" 4 "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " free"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " aligned_alloc"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " malloc"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " omp_target_alloc"  "usm_transform"  } } */
/* { dg-final { scan-tree-dump-not " omp_target_free"  "usm_transform"  } } */
