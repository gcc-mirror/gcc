/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

long set_work_pending_p;
_Bool set_work_pending() {
  _Bool __trans_tmp_1;
  long mask = 1, old = __atomic_fetch_or(&set_work_pending_p, mask, 0);
  __trans_tmp_1 = old & mask;
  return !__trans_tmp_1;
}
void __queue_work() {
  _Bool ret = set_work_pending();
  if (ret)
    __queue_work();
}

