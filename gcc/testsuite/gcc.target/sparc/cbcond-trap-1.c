/* { dg-do run { target cbcond_hw } } */
/* { dg-options "-O2 -mcpu=niagara4" } */

/* find_cond_trap used to emit the comparison of the conditional trap on top
   of a %icc value that was still live, because a cbcond jump does not read
   the CC.  */

struct S { unsigned short bt; unsigned char kind; unsigned bw; };

__attribute__((noinline)) void f (struct S *s)
{
  switch (s->bt)
    {
    case 1:   s->bw = 8;  s->kind = 1; break;
    case 2:   s->bw = 16; s->kind = 1; break;
    case 4:   s->bw = 32; s->kind = 1; break;
    case 8:   s->bw = 64; s->kind = 1; break;
    case 16:  s->bw = 16; s->kind = 2; break;
    case 32:  s->bw = 16; s->kind = 3; break;
    case 64:  s->bw = 32; s->kind = 3; break;
    case 128: s->bw = 64; s->kind = 3; break;
    case 256: s->bw = 8;  s->kind = 4; break;
    case 512: s->bw = 8;  s->kind = 5; break;
    default:  __builtin_trap ();
    }
}

int
main (void)
{
  struct S s = { 1, 0, 0 };
  f (&s);
  return s.bw == 8 ? 0 : 1;
}
