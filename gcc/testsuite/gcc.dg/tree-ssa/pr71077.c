/* PR c++/71077  */
/* { dg-do link { target { i?86-*-* x86_64-*-* } } }  */
/* { dg-require-effective-target lto } */
/* { dg-options "-O3 -flto -march=core-avx2" }  */

int *a;
int b, c, d, e;
int sched_analyze(void) {
 for (; b; b++) {
   c = 0;
   for (; c < 32; c++)
     if (b & 1 << c)
       a[b + c] = d;
 }
 return 0;
}

void schedule_insns(void) { e = sched_analyze(); }
int main(void) { schedule_insns(); }
