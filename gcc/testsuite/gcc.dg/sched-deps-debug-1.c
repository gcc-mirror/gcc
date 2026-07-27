/* A debug insn records a use in a reg_last entry that reg_last_in_use does not
   cover.  free_deps has to release those as well, or the reg_last array it
   hands back still points at the freed region's insn lists.  -fchecking=2
   checks an array before it is reused.  */

/* { dg-do compile } */
/* { dg-options "-O2 -g -fschedule-insns -fchecking=2" } */
/* { dg-require-effective-target scheduling } */

void f1 (double);
void f2 (int);

void
foo (int type, double xx)
{
  /* On the f2 path xx is dead, so what is left of it is a debug insn.  */
  if (type)
    f1 (xx);
  else
    f2 (type);
}

void
bar (int type)
{
  foo (type, 1.0);
}
