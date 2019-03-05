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
