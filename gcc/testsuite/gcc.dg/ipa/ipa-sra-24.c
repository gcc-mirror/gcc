/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized -Werror"  } */

int *ttmp_1;
_Bool pt_ins_tipdo, pq_ins_apd, pq_ins_tt2;
int gtrphdt;

void pl_ins(int, _Bool, _Bool);
inline void pt_ins(int *, _Bool apdo) {
  int list = *ttmp_1;
  pl_ins(list, apdo, pt_ins_tipdo);
}
void pq_ins(int *t) {
  if (pq_ins_tt2)
    pt_ins(t, pq_ins_apd);
}
int gtr_post_hd() {
  pq_ins(&gtrphdt);
  return 0;
}
