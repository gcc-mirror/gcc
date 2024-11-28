/* PR tree-optimization/117358 */

char a;
/* This attribute is bogus, snprintf isn't const.  Just verify we don't ICE on it.  */
int __attribute__((const)) snprintf (char *, __SIZE_TYPE__, const char *, ...);

long
foo (long d) 
{ 
  return snprintf (&a, d, ""); 
}

int
bar (void) 
{ 
  return foo (1); 
}
