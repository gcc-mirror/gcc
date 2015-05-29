/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern const unsigned char mode_ibit[];
extern const unsigned char mode_fbit[];
extern const signed char smode_ibit[];
extern const signed char smode_fbit[];

/* We use bit-and rather than modulo to ensure we're actually
   testing the desired match.pd pattern.  */
unsigned char
muufubar (int indx)
{
  int ret = (mode_fbit [indx] - mode_ibit [indx]) & 3;
  return ret;
}

signed char
msufubar (int indx)
{
  int ret = (mode_fbit [indx] - mode_ibit [indx]) & 3;
  return ret;
}

unsigned char
musfubar (int indx)
{
  int ret = (smode_fbit [indx] - smode_ibit [indx]) & 3;
  return ret;
}

signed char
mssfubar (int indx)
{
  int ret = (smode_fbit [indx] - smode_ibit [indx]) & 3;
  return ret;
}


unsigned char
puufubar (int indx)
{
  int ret = (mode_fbit [indx] + mode_ibit [indx]) & 3;
  return ret;
}

signed char
psufubar (int indx)
{
  int ret = (mode_fbit [indx] + mode_ibit [indx]) & 3;
  return ret;
}

unsigned char
pusfubar (int indx)
{
  int ret = (smode_fbit [indx] + smode_ibit [indx]) & 3;
  return ret;
}

signed char
pssfubar (int indx)
{
  int ret = (smode_fbit [indx] + smode_ibit [indx]) & 3;
  return ret;
}

/* The shortening patterns in match.pd should arrange to do the
   arithmetic in char modes and thus any casts to ints should
   have been removed.  */
/* { dg-final {scan-tree-dump-not "\\(int\\)" "optimized"} } */

/* We should have casted 4 operands from signed to unsigned char types.  */
/* { dg-final {scan-tree-dump-times "\\(unsigned char\\)" 8 "optimized" } } */

/* And two return values should have been casted from unsigned char to
   a normal char.  */
/* { dg-final {scan-tree-dump-times "\\(signed char\\)" 4 "optimized" } } */
