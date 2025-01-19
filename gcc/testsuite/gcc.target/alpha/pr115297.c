/* PR target/115297 */
/* { dg-do compile } */
/* { dg-options "" } */

enum { BPF_F_USER_BUILD_ID } __bpf_get_stack_size;
long __bpf_get_stack_flags, bpf_get_stack___trans_tmp_2;

void bpf_get_stack() {
  unsigned elem_size;
  int err = elem_size = __bpf_get_stack_flags ?: sizeof(long);
  if (__builtin_expect(__bpf_get_stack_size % elem_size, 0))
    bpf_get_stack___trans_tmp_2 = err;
}
