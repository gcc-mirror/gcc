/* Reduced from false positive in Linux kernel in
   drivers/xen/privcmd.c.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

typedef __SIZE_TYPE__ size_t;
typedef unsigned short __u16;
typedef unsigned int gfp_t;
void
kfree(const void* objp);

extern void *
__attribute__((__alloc_size__(1, 2)))
__attribute__((__malloc__))
kcalloc(size_t n, size_t size, gfp_t flags);

extern unsigned long
copy_from_user(void*, const void*, unsigned long);

typedef __u16 domid_t;
struct privcmd_dm_op_buf
{
  void* uptr;
  size_t size;
};
struct privcmd_dm_op
{
  domid_t dom;
  __u16 num;
};
static unsigned int privcmd_dm_op_max_num = 16;
long
privcmd_ioctl_dm_op(void* udata)
{
  struct privcmd_dm_op kdata;
  struct privcmd_dm_op_buf* kbufs;
  if (copy_from_user(&kdata, udata, sizeof(kdata)))
    return -14;
  if (kdata.num == 0)
    return 0;
  if (kdata.num > privcmd_dm_op_max_num)
    return -7;
  kbufs =
    kcalloc(kdata.num,  /* { dg-bogus "attacker-controlled value" }  */
            sizeof(*kbufs),
            (((gfp_t)(0x400u | 0x800u)) | ((gfp_t)0x40u) | ((gfp_t)0x80u)));
  if (!kbufs)
    return -12;
  kfree(kbufs);
  return 0;
}
