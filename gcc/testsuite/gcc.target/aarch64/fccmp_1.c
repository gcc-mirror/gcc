/* { dg-do compile } */
/* { dg-options "-O2 -fno-trapping-math -march=armv8.2-a+fp16" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* FCCMP and FCCMPE have half precision forms when FEAT_FP16 is available.
   Trapping behaviour is disabled so each conditional comparison is safe.  */

/*
** hf_lt:
**	fcmp	h0, h1
**	fccmp	h2, h3, 0, mi
**	cset	w0, mi
**	ret
*/
int
hf_lt (_Float16 a, _Float16 b, _Float16 c, _Float16 d)
{
  return (a < b) && (c < d);
}

/*
** hf_eq:
**	fcmp	h0, h1
**	fccmp	h2, h3, 0, eq
**	cset	w0, eq
**	ret
*/
int
hf_eq (_Float16 a, _Float16 b, _Float16 c, _Float16 d)
{
  return (a == b) && (c == d);
}

/* Exercise the reverse conditional-compare pattern.  */

/*
** hf_ior:
**	fcmp	h0, h1
**	fccmp	h2, h3, 8, pl
**	cset	w0, mi
**	ret
*/
int
hf_ior (_Float16 a, _Float16 b, _Float16 c, _Float16 d)
{
  return (a < b) || (c < d);
}
