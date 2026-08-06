/* { dg-do compile } */
/* { dg-options "-O2 -fchecking" } */

/* The partition holding the default definition of A2 also holds names of the
   store-motion temporaries of G0.  Walking the names of that partition gives
   its RTL the MEM_EXPR of a temporary, which the partition of that temporary
   has too, but the parameter gets its own MEM_EXPR back before any statement
   is expanded.  */

typedef unsigned long long v16u64 __attribute__((vector_size (128)));

v16u64 g0;
_Bool g9, ob13;
unsigned char g17;
static unsigned char g24 = 3;
void *a3;
__attribute__((cold)) void f8 (_Bool, unsigned char);

void
f30 (unsigned long long a0, _Bool a1, v16u64 a2)
{
  unsigned long long v8;
  _Bool c9;
cont1:
  a2 = g0;
  g0 = __builtin_shufflevector (g0, g0, 0, 1, 1, 4, 7, 2, 0, 8, 7, 7, 7, 8, 9,
				0, 1, 4);
  switch (a0)
    {
    case 201146615185186167:
      goto sw3;
    case 1:
      goto sw3;
    default:
      goto cont1;
    }
sw3:
  c9 = g24;
  goto bf66;
bf26:
  c9 = v8;
  a3 = &c9;
  if (c9)
    g0 = ~g0;
  if (a1)
    goto br51;
  goto bf66;
br32:
  if (ob13)
    goto br47;
  if (__builtin_expect_with_probability (g9, 0, 0.265))
    goto br32;
  goto bf66;
br47:
  f8 (a1, g17);
br51:
  a2 = g0;
  if (c9)
    goto bf26;
sw55:
  g0 = a2;
  return;
bf66:
  if (__builtin_expect_with_probability (c9, 0, 0.519))
    goto br32;
  goto sw55;
}
