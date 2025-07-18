/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized" } */

int foo(int);
enum {
  BPF_TRACE_RAW_TP,
  BPF_MODIFY_RETURN,
  BPF_LSM_MAC,
  BPF_TRACE_ITER,
  BPF_LSM_CGROUP
};
int btf_get_kernel_prefix_kind_prefix, obj_1, attach_name___trans_tmp_1;
char attach_name_fn_name;
void attach_name(int attach_type)
{
  int mod_len;
  char mod_name = attach_name_fn_name;
  if (attach_name_fn_name)
    mod_len = mod_name;
  for (; obj_1;) {
    if (mod_name && foo(mod_len))
      continue;
    switch (attach_type) {
    case BPF_TRACE_RAW_TP:
    case BPF_LSM_MAC:
    case BPF_LSM_CGROUP:
      btf_get_kernel_prefix_kind_prefix = 1;
    case BPF_TRACE_ITER:
      attach_name_fn_name = 2;
    }
    if (attach_name___trans_tmp_1)
      return;
  }
}
