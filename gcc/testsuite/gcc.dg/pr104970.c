/* { dg-do compile } */
/* { dg-options "-O1" } */

__inline void
memset2(void *__dest, int __ch, long __len) {
  long __trans_tmp_1 = __builtin_dynamic_object_size(__dest, 0);
  __builtin___memset_chk(__dest, __ch, __len, __trans_tmp_1);
}

void
mleye(int l, double E[][l]) { memset2(E, 0, sizeof(double)); }


