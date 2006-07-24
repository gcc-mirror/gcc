/* { dg-do compile } */

void multiply(float *data, const float *op, unsigned int size)
{
    unsigned int i;
    for (i=0; i<size; ++i)
    {
       data[i] *= op[i];
    }
}

/* { dg-final { scan-tree-dump "vectorized 0 loops in function" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
