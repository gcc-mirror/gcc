/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern unsigned int strlen (const char *) __attribute__ ((__pure__));

void
foo (const char *str)
{
  unsigned int a = strlen (str);
  unsigned int b = strlen (str);
  if (a != b)
    link_error ();
}
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
