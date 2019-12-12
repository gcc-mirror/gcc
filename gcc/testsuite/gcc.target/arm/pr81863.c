/* testsuite/gcc.target/arm/pr48183.c */
/* { dg-do compile } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mslow-flash-data" } } */
/* { dg-skip-if "-mpure-code and -mword-relocations incompatible" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-O2 -mword-relocations -march=armv7-a -marm" } */
/* { dg-final { scan-assembler-not "\[\\t \]+movw" } } */

int a, d, f;
long b;
struct ww_class {
  int stamp;
} c;
struct stress {
  int locks;
  int nlocks;
};
void *e;
int atomic_add_return_relaxed(int *p1) {
  __builtin_prefetch(p1);
  return a;
}
void atomic_long_inc_return_relaxed(int *p1) {
  int *v = p1;
  atomic_add_return_relaxed(v);
}
void ww_acquire_init(struct ww_class *p1) {
  atomic_long_inc_return_relaxed(&p1->stamp);
}
void ww_mutex_lock();
int *get_random_order();
void stress_inorder_work() {
  struct stress *g = e;
  int h = g->nlocks;
  int *i = &g->locks, *j = get_random_order();
  do {
    int n;
    ww_acquire_init(&c);
  retry:
    for (n = 0; n < h; n++)
      ww_mutex_lock(i[j[n]]);
    f = n;
    if (d)
      goto retry;
  } while (b);
}

