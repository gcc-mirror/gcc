/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue" } */
typedef struct a b;
typedef double c;
struct a {
  b *d;
  b *e;
};
struct f {
  c g;
};
inline bool h(c i, b *m) {
  b *j = m->e;
  for (; m->e; j = j->d)
    if (h(i, j))
      return 0;
  return m;
}
bool k() {
  f *l;
  b *n;
 return h(l->g, n);
}
/* { dg-final { scan-rtl-dump "The components we wrap separately are \\\[sep 3 4\\\]" "pro_and_epilogue" { target { ia32 } } } } */
/* { dg-final { scan-rtl-dump "The components we wrap separately are \\\[sep 40 41 42 43\\\]" "pro_and_epilogue" { target { ! ia32 } } } } */
