/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple -fdump-tree-ompdevlow" } */

#define N 256

void
f (int a[], int num)
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

/* For configurations with offloading, we expect one "pragma omp target"
   with "device(num)" for each target_device selector that specifies
   "device_num(num)".  Without offloading, there should be zero as the
   resolution happens during gimplification.  */
/* { dg-final { scan-tree-dump-times "pragma omp target\[^\\n\]* device\\(num" 3 "gimple" { target offloading_enabled } } } */
/* { dg-final { scan-tree-dump-times "pragma omp target\[^\\n\]* device\\(num" 0 "gimple" { target { ! offloading_enabled } } } } */

/* For configurations with offloading, expect one OMP_TARGET_DEVICE_MATCHES
   for each kind/arch/isa selector.  These are supposed to go away after
   ompdevlow.  */
/* { dg-final { scan-tree-dump-times "OMP_TARGET_DEVICE_MATCHES" 9 "gimple" { target offloading_enabled } } } */
/* { dg-final { scan-tree-dump-times "OMP_TARGET_DEVICE_MATCHES" 0 "gimple" { target { ! offloading_enabled } } } } */
/* { dg-final { scan-tree-dump-times "OMP_TARGET_DEVICE_MATCHES" 0 "ompdevlow" { target offloading_enabled } } } */
