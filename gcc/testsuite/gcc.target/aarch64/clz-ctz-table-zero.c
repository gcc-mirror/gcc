/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* AArch64 defines the value of CLZ and CTZ at zero, so the table lookups
   should still become calls that carry it, and a guard against zero that
   yields that same value should fold away.  */

static const unsigned TC[32] = {
  0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
  8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31
};

static const unsigned char TT[32] = {
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

unsigned
clz_fb (unsigned v)
{
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  return 31 - TC[(v * 0x07C4ACDDU) >> 27];
}

unsigned
ctz_fb (unsigned v)
{
  return TT[((v & -v) * 0x077CB531U) >> 27];
}

/*
** clz_g:
**	clz	w0, w0
**	ret
*/

unsigned
clz_g (unsigned v)
{
  return v ? clz_fb (v) : 32;
}

/*
** ctz_g:
** (
**	rbit	w0, w0
**	clz	w0, w0
** |
**	ctz	w0, w0
** )
**	ret
*/

unsigned
ctz_g (unsigned v)
{
  return v ? ctz_fb (v) : 32;
}

/* { dg-final { scan-tree-dump-times "\\.CLZ \\(" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\.CTZ \\(" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\.CLZ \\(\[^\\n,\]*, 32\\)" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\.CTZ \\(\[^\\n,\]*, 32\\)" 2 "forwprop1" } } */
