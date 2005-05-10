/* Test for computed jump into cond_expr: bug 17913.  */
void f (void) 
{ 
  void *p = &&a;
  1 ? 1 : ({ a : 1; }); 
  goto *p;
}
