/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=1" } */

/* Based on strict-overflow-5.c.  */

/* We can only unroll when using strict overflow semantics.  But we
   don't issue a warning for relying on undefined overflow in
   loops.  */

int foo (int i)
{
  int index;
  int r=0;
 
  for (index = i; index <= i+4; index+=2)
    r++;
 
  return r;
}
