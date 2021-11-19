/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-O3 -mbig-endian -mcpu=hs38" } */

struct a {};
struct b {
  int c;
  int d;
};

struct {
  struct a e;
  struct b f[];
} g;
short h;

extern void bar (int *);

int foo(void)
{
  struct b *a;
  for (;;)
    {
      a = &g.f[h];
      bar(&a->d);
    }
}

/* { dg-final { scan-assembler "dmach" } } */
