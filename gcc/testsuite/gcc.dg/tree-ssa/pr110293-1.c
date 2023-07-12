/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */

_Bool eqeq0(unsigned x)
{
  return x == (x == 0);
}
_Bool eqeq1(unsigned x)
{
  return x == (x == 1);
}
_Bool eqeq2(unsigned x)
{
  return x == (x == 2);
}

_Bool neeq0(unsigned x)
{
  return x != (x == 0);
}
_Bool neeq1(unsigned x)
{
  return x != (x == 1);
}
_Bool neeq2(unsigned x)
{
  return x != (x == 2);
}

_Bool eqne0(unsigned x)
{
  return x == (x != 0);
}
_Bool eqne1(unsigned x)
{
  return x == (x != 1);
}
_Bool eqne2(unsigned x)
{
  return x == (x != 2);
}

_Bool nene0(unsigned x)
{
  return x != (x != 0);
}
_Bool nene1(unsigned x)
{
  return x != (x != 1);
}
_Bool nene2(unsigned x)
{
  return x != (x != 2);
}

/* All of these functions should have removed the inner most comparison which
   means all of the conversions from bool to unsigned should have been removed too. */
/* { dg-final { scan-tree-dump-not "nop_expr," "optimized"} } */
