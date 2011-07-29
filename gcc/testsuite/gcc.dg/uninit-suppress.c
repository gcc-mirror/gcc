/* { dg-do compile } */
/* { dg-options "-fno-tree-ccp -fno-tree-vrp -O2 -Wuninitialized -Wno-maybe-uninitialized" } */
void blah();
int gflag;

void foo()
{
   int v;
   if (gflag)
     v = 10;

   blah(); /* *gflag may be killed, but compiler won't know */

   if (gflag)
    bar(v);   /* { dg-bogus "uninitialized" "should be suppressed" } */
}
