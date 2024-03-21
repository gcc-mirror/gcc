/* Reduced from false positive in Linux kernel's fs/ceph/ioctl.c: */

__extension__ typedef unsigned long long __u64;

struct ceph_ioctl_layout
{
  __u64 stripe_unit, object_size;
};
static long
__validate_layout(struct ceph_ioctl_layout* l)
{
  if ((l->object_size & ~(~(((1UL) << 12) - 1))) ||
      (l->stripe_unit & ~(~(((1UL) << 12) - 1))) ||
      ((unsigned)l->stripe_unit != 0 &&
       ((unsigned)l->object_size % (unsigned)l->stripe_unit))) /* { dg-bogus "use of attacker-controlled value 'l.stripe_unit' as divisor without checking for zero" "PR analyzer/113619" } */
    return -22;
  return 0;
}

long
__attribute__((tainted_args))
ceph_ioctl_set_layout_policy(struct ceph_ioctl_layout l)
{
  int err;
  err = __validate_layout(&l);
  if (err)
    return err;
  return err;
}
