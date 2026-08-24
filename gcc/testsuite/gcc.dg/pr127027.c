/* { dg-do compile } */
/* { dg-options "-O2" } */

struct extent_hooks_s {
  _Bool dalloc;
};
typedef struct {
  void *repr;
} atomic_p_t;
atomic_p_t atomic_load_p_a;
enum { extent_state_muzzy } ehooks_are_default() {
  _Bool __trans_tmp_3;
  struct extent_hooks_s *__trans_tmp_2;
  if (ehooks_are_default()) {
    void *result;
    __atomic_load(&atomic_load_p_a, &result, 0);
    __trans_tmp_2 = result;
    __trans_tmp_3 = __trans_tmp_2->dalloc;
  }
  if (__trans_tmp_3)
    __builtin_unreachable();
}
