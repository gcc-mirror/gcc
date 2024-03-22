/* Reduced from false positive in Linux kernel in
   drivers/platform/x86/intel/speed_select_if/isst_tpmi_core.c.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

typedef unsigned char __u8;
typedef unsigned short __u16;
extern unsigned int __max_logical_packages;
extern unsigned long
copy_from_user(void* to, const void* from, unsigned long n);
extern unsigned long
copy_to_user(void* to, const void* from, unsigned long n);
struct isst_tpmi_instance_count
{
  __u8 socket_id;
  __u8 count;
  __u16 valid_mask;
};
struct tpmi_per_power_domain_info
{
  void* sst_base;
};
struct tpmi_sst_struct
{
  int number_of_power_domains;
  struct tpmi_per_power_domain_info* power_domain_info;
};
struct tpmi_sst_common_struct
{
  int max_index;
  struct tpmi_sst_struct** sst_inst;
};
static struct tpmi_sst_common_struct isst_common;
int
isst_if_get_tpmi_instance_count(void* argp)
{
  struct isst_tpmi_instance_count tpmi_inst;
  struct tpmi_sst_struct* sst_inst;
  int i;
  if (copy_from_user(&tpmi_inst, argp, sizeof(tpmi_inst)))
    return -14;
  if (tpmi_inst.socket_id >= (__max_logical_packages))
    return -22;
  tpmi_inst.count =
    isst_common.sst_inst[tpmi_inst.socket_id]->number_of_power_domains; /* { dg-bogus "use of attacker-controlled value as offset without upper-bounds checking" } */
  sst_inst = isst_common.sst_inst[tpmi_inst.socket_id];
  tpmi_inst.valid_mask = 0;
  for (i = 0; i < sst_inst->number_of_power_domains; ++i) {
    struct tpmi_per_power_domain_info* pd_info;
    pd_info = &sst_inst->power_domain_info[i];
    if (pd_info->sst_base)
      tpmi_inst.valid_mask |= ((((1UL))) << (i));
  }
  if (copy_to_user(argp, &tpmi_inst, sizeof(tpmi_inst)))
    return -14;
  return 0;
}
