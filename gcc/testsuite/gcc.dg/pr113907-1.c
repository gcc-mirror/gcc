/* PR middle-end/113907 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-minline-all-stringops" { target i?86-*-* x86_64-*-* } } */

static inline int
foo (int len, void *indata, void *outdata)
{
  if (len < 0 || (len & 7) != 0)
    return 0;
  if (len != 0 && indata != outdata)
    __builtin_memcpy (outdata, indata, len);
  return len;
}

static inline int
bar (int len, void *indata, void *outdata)
{
  if (len < 0 || (len & 1) != 0)
    return 0;
  if (len != 0 && indata != outdata)
    __builtin_memcpy (outdata, indata, len);
  return len;
}

int (*volatile p1) (int, void *, void *) = foo;
int (*volatile p2) (int, void *, void *) = bar;

__attribute__((noipa)) int
baz (int len, void *indata, void *outdata)
{
  if ((len & 6) != 0)
    bar (len, indata, outdata);
  else
    foo (len, indata, outdata);
}

struct S { char buf[64]; } s __attribute__((aligned (8)));

int
main ()
{
  for (int i = 0; i < 64; ++i)
    s.buf[i] = ' ' + i;
  p2 (2, s.buf, s.buf + 33);
  for (int i = 0; i < 64; ++i)
    if (s.buf[i] != ' ' + ((i >= 33 && i < 35) ? i - 33 : i))
      __builtin_abort ();
}
