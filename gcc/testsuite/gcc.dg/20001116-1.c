/* This looks like a warning test, but it's actually a regression test for a
   nasty ICE due to messed up parser context.  Problem originally found
   during bootstrap; this is synthetic.  -zw  */
/* { dg-do compile } 
   { dg-options -Wempty-body } */

void foo (int x)
{
  if (x)
    ;	/* { dg-warning "empty body in an" } */
}
