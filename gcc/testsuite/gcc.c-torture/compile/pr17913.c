/* Test for computed jump into cond_expr: bug 17913.  */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

void f (void) 
{ 
  void *p = &&a;
  1 ? 1 : ({ a : 1; }); 
  goto *p;
}
