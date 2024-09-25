/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O2 -fnon-call-exceptions -fprofile-arcs -finstrument-functions -fno-tree-copy-prop" } */

_BitInt(5) b5;

char c;
int i;
_BitInt(129) b129;

void
foo(_BitInt(128) b128)
{
l50:
  b128 %= b128 - b129;
l64:
  b128 %= c;
  if (__builtin_add_overflow(i, 0, &c))
    goto l50;
  if (__builtin_sub_overflow(c, 0, &b5))
    goto l64;
}
