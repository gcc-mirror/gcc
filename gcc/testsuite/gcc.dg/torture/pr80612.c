/* PR tree-optimization/80612 */
/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

struct obstack *a;
struct obstack {
  union {
    void *plain;
    void (*extra)();
  } chunkfun;
} fn1(void p4()) {
  a->chunkfun.plain = p4;
  a->chunkfun.extra(a);
}
void fn2(int) __attribute__((__alloc_size__(1)));
void fn3() { fn1(fn2); }

/* { dg-prune-output "attribute ignored" } */
