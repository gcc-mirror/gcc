/* { dg-do compile } */
/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler-not "stmia\[\\t \]*r3!\[^\\n]*r3" } } */
typedef struct {
  void *t1, *t2, *t3;
} z;
extern volatile int y;
static inline void foo(z *x) {
  x->t1 = &x->t2;
  x->t2 = ((void *)0);
  x->t3 = &x->t1;
}
extern z v;
void bar (void) {
   y = 0;
   foo(&v);
}
