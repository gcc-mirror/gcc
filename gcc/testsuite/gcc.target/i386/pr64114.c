/* { dg-do compile } */
/* { dg-options "-O -mstringop-strategy=vector_loop -mno-push-args" } */

extern float *ptrs[];
extern int incs[];

struct big
{
  int i[0x10000];
};

extern void bar (struct big b);

struct big
foo (int n)
{
  struct big b;

  int inc14 = incs[15], inc16 = n, inc17 = n, inc19 = incs[3];
  float *ptr0 = ptrs[1], *ptr14 = ptrs[14], *ptr16 = ptrs[16];
  while (n--)
    *ptr0 += *ptr14 += inc14 += *ptr16 += inc16 += inc17 += inc19, bar (b);
  return b;
}
