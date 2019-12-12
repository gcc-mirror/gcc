/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

void f2 (void);

int main (void)
{
  int i;
  for (i = 0; i < 10; i++)
    f2 ();
}

/* { dg-final { scan-tree-dump-times "!= 0" 5 "ivopts" { target { ! powerpc*-*-* } } } } */
/* More debug information emitted for doloop on powerpc.  */
/* { dg-final { scan-tree-dump-times "!= 0" 6 "ivopts" { target { powerpc*-*-* } } } } */
