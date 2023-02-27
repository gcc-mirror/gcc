/* Reduced from infoleak false positive in drivers/vfio/vfio_iommu_type1.c  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

typedef unsigned int u32;
typedef unsigned long long u64;

unsigned long
copy_from_user(void *to, const void *from, unsigned long n);

unsigned long
copy_to_user(void *to, const void *from, unsigned long n);

struct vfio_iommu_type1_info {
  u32 argsz;
  u32 flags;
  u64 iova_pgsizes;
  u32 cap_offset;
  /* bytes 20-23 are padding.  */
};

int vfio_iommu_type1_get_info(unsigned long arg)
{
  struct vfio_iommu_type1_info info;
  unsigned long minsz = 16;

  if (copy_from_user(&info, (void *)arg, 16))
    return -14;

  if (info.argsz < 16)
    return -22;

  if (info.argsz >= 20) {
    minsz = 20;
    info.cap_offset = 0;
  }

  /* The padding bytes (20-23, but applicable just for targets with padding) are
     uninitialized, but can't be written back, since minsz is either 16 or 20.  */
  return copy_to_user((void *)arg, &info, minsz) ? -14 : 0; /* { dg-bogus "exposure" "" { xfail { ! default_packed } } } */
  // TODO: false +ve due to not handling minsz being either 16 or 20
}
