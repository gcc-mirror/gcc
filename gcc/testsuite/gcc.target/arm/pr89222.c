/* { dg-do compile } */
/* { dg-options "-O2" } */

void g (void);

void f1 (int x)
{
  if (x != (int) g + 3)
    return;
  g();
}

void (*a2)(void);

void f2 (void)
{
  a2 = &g + 3;
}

typedef void (*__sighandler_t)(int);
void handler (int);

void f3 (int x)
{
  __sighandler_t h = &handler;
  if (h != (__sighandler_t) 2 && h != (__sighandler_t) 1)
    h (x);
}

/* { dg-final { scan-assembler-times {add(?:s)?\tr[0-9]+, r[0-9]+, #3} 2 } } */
/* { dg-final { scan-assembler-not {.word\tg\+3} } } */
/* { dg-final { scan-assembler-not {.word\thandler-1} } } */
