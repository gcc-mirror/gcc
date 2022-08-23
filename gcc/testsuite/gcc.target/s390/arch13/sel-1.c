/* { dg-do compile } */

int
sel32 (int a, int then, int els)
{
  return a > 42 ? then : els;
}

/* FIXME: This currently fails since ifcvt considers that combination
   too expensive.  THe reason is that additional load instructions
   emitted by ifcvt are part of the costs although these should get
   removed later.  */
/* { dg-final { scan-assembler-times "\tselr(?:h|le)\t" 1 } } */

long long
sel64 (int a, long long then, long long els)
{
  return a > 42 ? then : els;
}

/* { dg-final { scan-assembler-times "\tselgr(?:h|le)\t" 1 } } */
