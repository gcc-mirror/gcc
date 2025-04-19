/* PR tree-optimization/118947 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -fdump-tree-forwprop1-details" } */
/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */

void* aaa();
void* bbb()
{
    char buf[1025] = {};
    /*  Tha call to aaa should not matter and clobber buf. */
    void* ret = aaa();
    __builtin_memcpy(ret, buf, sizeof(buf));
    return ret;
}

