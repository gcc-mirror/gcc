/* { dg-do compile */
/* { dg-additional-options "-fdump-tree-optimized" } */

/* In configurations without offloading configured, we can resolve many
   instances of the target_device context selector at gimplification time
   instead of waiting until late resolution.  */

/* Device 0 may be either the host or an offloading device, in configurations
   that support them.  */
void
f1 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(0), kind(host)}		\
	: parallel for)						\
  default (error at(execution) message("f1 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f1 match failed" "optimized" { target { ! offloading_enabled } } } } */
/* { dg-final { scan-tree-dump "f1 match failed" "optimized" { target offloading_enabled } } } */

/* Device -1 is always the host, even in offloading configurations.  */
void
f2 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(-1), kind(host)}		\
	: parallel for)						\
  default (error at(execution) message("f2 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f2 match failed" "optimized" } } */

/* Constant device numbers < -1 cause the match to fail, even in offloading
   configurations.  */
void
f3 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(-42), kind(host)}		\
	: error at(execution) message("f3 match failed"))	\
  default (parallel for)
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f3 match failed" "optimized" } } */

/* In a non-offloading configuration, devices > 0 are always invalid and
   will cause the match to fail.  In an offload configuration, we don't
   know at compile-time if the device number is valid or if it refers to
   a host or offload device.  */
void
f4 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(42), kind(host)}		\
	: error at(execution) message("f4 match failed"))	\
  default (parallel for)
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f4 match failed" "optimized" { target { ! offloading_enabled } } } } */
/* { dg-final { scan-tree-dump "f4 match failed" "optimized" { target offloading_enabled } } } */

extern int omp_get_initial_device (void);
extern int omp_get_default_device (void);
extern int omp_get_num_devices (void);
extern int omp_get_device_num (void);

/* On a non-offloading configuration, omp_get_default_device() always refers
   to the host.  With offloading, we know it's a valid device number but
   not whether it's the host.  */
void
f5 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(omp_get_default_device ()), kind(host)} \
	: parallel for)						\
  default (error at(execution) message("f5 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f5 match failed" "optimized" { target { ! offloading_enabled } } } } */
/* { dg-final { scan-tree-dump "f5 match failed" "optimized" { target offloading_enabled } } } */

/* omp_get_initial_device() always refers to the host, whether offloading is
   configured or not.  */
void
f6 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(omp_get_initial_device ()), kind(host)} \
	: parallel for)						\
  default (error at(execution) message("f6 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f6 match failed" "optimized" } } */

/* omp_get_num_devices() returns the number of non-host offload devices,
   therefore using it as a device number always refers to the host, whether
   offloading is enabled or not.  */
void
f7 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(omp_get_num_devices ()), kind(host)} \
	: parallel for)						\
  default (error at(execution) message("f7 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f7 match failed" "optimized" } } */

/* omp_get_device_num() always returns a valid device number but we don't
   know whether it's the host or offload device, in configurations that
   support offloading.  */
void
f8 (int n, double* a, double s)
{
#pragma omp metadirective					\
  when (target_device={device_num(omp_get_device_num ()), kind(host)} \
	: parallel for)						\
  default (error at(execution) message("f8 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}
/* { dg-final { scan-tree-dump-not "f8 match failed" "optimized" { target { ! offloading_enabled } } } } */
/* { dg-final { scan-tree-dump "f8 match failed" "optimized" { target offloading_enabled } } } */

