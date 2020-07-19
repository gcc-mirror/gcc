// { dg-do compile }
// { dg-final { scan-assembler-not "_d_arraycopy" } }

void test1()
{
    int[10] a1 = void;
    int[10] a2 = void;
    a1[] = a2[];
}

void test2(int[] a1, int[] a2)
{
    a1[] = a2[];
}
