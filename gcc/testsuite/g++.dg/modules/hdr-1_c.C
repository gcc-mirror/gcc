// { dg-additional-options {-fmodules-ts} }

import "hdr-1_b.H";

int foo (frob *p)
{
  return p->field;
}

int foo (FROB<2> *p)
{
  return p->val;
}

