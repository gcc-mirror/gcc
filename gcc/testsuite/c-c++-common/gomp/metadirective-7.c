/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 256

void f (int a[], int num)
{
  int i;

  #pragma omp metadirective \
      when (target_device={device_num(num), kind("gpu"), arch("nvptx")}: \
	    target parallel for map(tofrom: a[0:N])) \
      when (target_device={device_num(num), kind("gpu"), \
			   arch("amdgcn"), isa("gfx906")}: \
	    target parallel for) \
      when (target_device={device_num(num), kind("cpu"), arch("x86_64")}: \
	    parallel for)
    for (i = 0; i < N; i++)
      a[i] += i;

  #pragma omp metadirective \
      when (target_device={kind("gpu"), arch("nvptx")}: \
	    target parallel for map(tofrom: a[0:N]))
    for (i = 0; i < N; i++)
      a[i] += i;
}

/* { dg-final { scan-tree-dump "__builtin_GOMP_evaluate_target_device \\(num, &\"gpu\"\\\[0\\\], &\"amdgcn\"\\\[0\\\], &\"gfx906\"\\\[0\\\]\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_evaluate_target_device \\(num, &\"gpu\"\\\[0\\\], &\"nvptx\"\\\[0\\\], 0B\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_evaluate_target_device \\(num, &\"cpu\"\\\[0\\\], &\"x86_64\"\\\[0\\\], 0B\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_evaluate_target_device \\(-1, &\"gpu\"\\\[0\\\], &\"nvptx\"\\\[0\\\], 0B\\)" "gimple" } } */
