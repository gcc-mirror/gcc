/* PR debug/36690 */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf-2 -dA" } */

int cnt;

void
bar (int i)
{
  cnt += i;
}

void
foo (int i, int j)
{
  if (j)
    {
      bar (i + 1);
      goto f1;
    }
  bar (i + 2);
  goto f2;
f1:
  if (i > 10)
    goto f3;
f2:
  if (i > 40)
    goto f4;
  else
    goto f5;
f3:
  bar (i);
f4:
  bar (i);
f5:
  bar (i);
}

int
main (void)
{
  foo (0, 1);
  foo (11, 1);
  foo (21, 0);
  foo (41, 0);
  return 0;
}

/* { dg-final { scan-assembler "pr36690-3.c:19" } } */
/* { dg-final { scan-assembler "pr36690-3.c:22" } } */
/* { dg-final { scan-assembler "pr36690-3.c:25" } } */
/* { dg-final { scan-assembler "pr36690-3.c:28" } } */
/* { dg-final { scan-assembler "pr36690-3.c:30" } } */
