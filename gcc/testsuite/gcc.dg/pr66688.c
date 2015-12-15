/* PR tree-optimization/66688 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-reorder-blocks -fcompare-debug" } */

struct fdt_header { unsigned magic; } *a;

int d;

int
__fswab32 (int p1)
{
  return __builtin_bswap32 (p1);
}

void
fdt_set_magic (int p1)
{
  struct fdt_header *b = a;
  b->magic = __builtin_constant_p (p1) ? : __fswab32 (p1);
}

int
_fdt_sw_check_header ()
{
  int c = ((struct fdt_header *) 1)->magic;
  if (c)
    return 1;
  return 0;
}

int
fdt_finish ()
{
  if (_fdt_sw_check_header ())
    if (d)
      return 0;
  fdt_set_magic (0);
  return 0;
}
