/* { dg-do compile } */
/* { dg-options "-O2 -mstringop-strategy=rep_4byte" } */

extern unsigned x[];
void 
foo (void)
{
  __builtin_memset(x, 0, 847);
}
