/* REQUIRED_ARGS: -betterC
   PERMUTE_ARGS:
*/

import core.stdc.stdio;

extern (C) int main(char** argv, int argc) {
    printf("hello world\n");
    int[3] a;
    foo(a[], 3);
    return 0;
}

int foo(int[] a, int i)
{
    return a[i];
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=19234
void issue19234()
{
    static struct A {}
    A[10] a;
    A[10] b;
    b[] = a[];
}
