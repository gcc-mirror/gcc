/* PR tree-optimization/78408 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -fdump-tree-forwprop1-details" } */
/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */

void* aaa();
void* bbb()
{
    void* ret = aaa();
    char buf[1025] = {};
    __builtin_memcpy(ret, buf, sizeof(buf));
    return ret;
}

