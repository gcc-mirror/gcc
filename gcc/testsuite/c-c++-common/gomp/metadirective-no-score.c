/* { dg-do compile { target x86_64-*-* } } */
/* { dg-additional-options "-foffload=disable" } */

/* This test is expected to fail with compile-time errors:
   "A trait-score cannot be specified in traits from the construct,
   device or target_device trait-selector-sets."  */

/* Define this to avoid dependence on libgomp header files.  */

#define omp_initial_device -1

void
f1 (int n, double *a, double s)
{
#pragma omp metadirective		\
  when (device={kind (score(5) : host)} \
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

void
f2 (int n, double *a, double s)
{
#pragma omp metadirective					      \
  when (device={kind (host), arch (score(6) : x86_64), isa (avx512f)} \
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

void
f3 (int n, double *a, double s)
{
#pragma omp metadirective					\
  when (device={kind (host), arch (score(6) : x86_64),		\
		  isa (score(7): avx512f)}			\
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-3 } */
  /* { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-3 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

void
f4 (int n, double *a, double s)
{
#pragma omp metadirective						\
  when (target_device={device_num (score(42) : omp_initial_device),	\
			 kind (host)}					\
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-3 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

void
f5 (int n, double *a, double s)
{
#pragma omp metadirective				\
  when (target_device={device_num(omp_initial_device),	\
			 kind (score(5) : host)}	\
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-2 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

void
f6 (int n, double *a, double s)
{
#pragma omp metadirective					      \
  when (target_device={device_num(omp_initial_device), kind (host),   \
			 arch (score(6) : x86_64), isa (avx512f)}     \
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-2 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

void
f7 (int n, double *a, double s)
{
#pragma omp metadirective						\
  when (target_device={device_num(omp_initial_device), kind (host),	\
			 arch (score(6) : x86_64),			\
			 isa (score(7): avx512f)}			\
	: parallel for)
  /* { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-3 } */
  /* { dg-error ".score. cannot be specified in traits in the .target_device. trait-selector-set" "" { target *-*-*} .-3 } */
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
