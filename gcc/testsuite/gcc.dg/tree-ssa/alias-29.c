/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-add-options bind_pic_locally } */

union X {
    int i;
    void *p;
};
void bar (int);

int * __attribute__((noinline,noclone))
baz (int *p) { return p; }

void foo (union X *x)
{
  struct Y { int i; } ystruct = {};
  ystruct.i = * baz (&ystruct.i);
  bar (x->i);
}

/* DSE and then DCE should be able to remove all uses of ystruct.
   Formerly the union access for the parameter to bar let 'anything'
   escape which made the call to bar possibly use ystruct and thus
   prevent the store to ystruct.i from being eliminated.  The call to
   baz makes sure that ystruct has its address taken.  */

/* { dg-final { scan-tree-dump-not "ystruct" "optimized" } } */
