/* PR middle-end/29726 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

void foo (void);
#if(__SIZEOF_INT__ >= 4) 
int test1 (int a)
#else
int test1 (long a)
#endif

{
  if ((a >> 3) & 134217728)
    foo ();
}

#if(__SIZEOF_INT__ >= 4) 
int test2 (unsigned int b)
#else
int test2 (unsigned long b)
#endif
{
  if ((b >> 3) & 134217728)
    foo ();
}

/* { dg-final { scan-tree-dump-times "\\(a \& 1073741824\\) != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(b \& 1073741824\\) != 0" 1 "original" } } */
