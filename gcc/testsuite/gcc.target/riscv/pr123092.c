/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zicbop -mabi=lp64d" } */
struct a {
  char *b;
} d;
int e;
int c(struct a *);
void f() {
  __builtin_prefetch(d.b + 64);
  if (e)
    c(&d);
}
