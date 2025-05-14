/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

char string[1020] __attribute__((aligned(1)));

char * find(int n, char c)
{
    for (int i = 0; i < n; i++) {
        if (string[i] == c)
            return &string[i];
    }
    return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "Alignment of access forced using peeling" "vect" } } */
/* { dg-final { scan-tree-dump "force alignment of string" "vect" } } */
