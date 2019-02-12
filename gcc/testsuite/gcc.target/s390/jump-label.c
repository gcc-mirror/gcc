/* Test jdd constraint, which is used for linux kernel jump labels.  */

/* { dg-do link } */
/* { dg-options "-O2 -fPIC -shared" } */

__attribute__ ((visibility ("default"))) extern int i;

void f (void)
{
  asm goto (".pushsection foo\n"
#if defined(__s390x__)
            ".quad %0-.\n"
#else
            ".long %0-.\n"
#endif
            ".popsection\n"
            : : "jdd" (&i) : : l);
l:;
}
