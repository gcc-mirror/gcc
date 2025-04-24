/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -fdump-tree-vect-details" } */
int a[9*9];
bool b[9];
void test()
{
        for (int i = 0; i < 9; i++)
        {
                b[i] = a[i*9] != 0;
        }
}

/* { dg-final { scan-tree-dump "loop vectorized using 8 byte vectors" "vect" } } */
