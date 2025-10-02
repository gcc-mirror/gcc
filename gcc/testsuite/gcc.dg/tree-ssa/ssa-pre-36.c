/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

static unsigned long *
min_element(unsigned long* array, unsigned long size)
{
    unsigned long* min = &array[0];

    for (unsigned long i = 1; i < size; ++i)
        if (array[i] < *min)
            min = &array[i];
    
    return min;
}

unsigned long
min(unsigned long* array, unsigned long size)
{
    return *min_element(array, size);
}

/* We want to hoist the *min dereference before the loop.  */
/* { dg-final { scan-tree-dump "HOIST inserted: 1" "pre" } } */
/* { dg-final { scan-tree-dump-times "= \\\*" 2 "pre" } } */
