/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

char string[1020];

char * find(int n, char c)
{
    for (int i = 1; i < n; i++) {
        if (string[i] == c)
            return &string[i];
    }
    return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" } } */
