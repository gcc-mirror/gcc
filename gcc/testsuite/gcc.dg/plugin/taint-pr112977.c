/* Reduced from false positive in Linux kernel in
   drivers/scsi/aacraid/aachba.c.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

typedef unsigned char u8;
typedef unsigned int u32;

extern unsigned long
copy_from_user(void* to, const void* from, unsigned long n);

struct fsa_dev_info
{
  u8 valid;
  u8 deleted;
};
struct aac_dev
{
  int maximum_num_containers;
  struct fsa_dev_info* fsa_dev;
};
struct aac_delete_disk
{
  u32 disknum;
  u32 cnum;
};
int
force_delete_disk(struct aac_dev* dev, void* arg)
{
  struct aac_delete_disk dd;
  struct fsa_dev_info* fsa_dev_ptr;
  fsa_dev_ptr = dev->fsa_dev;
  if (!fsa_dev_ptr)
    return -16;
  if (copy_from_user(&dd, arg, sizeof(struct aac_delete_disk)))
    return -14;
  if (dd.cnum >= dev->maximum_num_containers)
    return -22;
  fsa_dev_ptr[dd.cnum].deleted = 1;
  fsa_dev_ptr[dd.cnum].valid = 0; /* { dg-bogus "use of attacker-controlled value as offset without upper-bounds checking" } */
  return 0;
}
