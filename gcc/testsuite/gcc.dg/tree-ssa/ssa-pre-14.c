/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern __SIZE_TYPE__ strlen (const char *) __attribute__ ((__pure__));
extern void link_error (void);

void
foo (const char *str)
{
  __SIZE_TYPE__ a = strlen (str);
  __SIZE_TYPE__ b = strlen (str);
  if (a != b)
    link_error ();
}
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
