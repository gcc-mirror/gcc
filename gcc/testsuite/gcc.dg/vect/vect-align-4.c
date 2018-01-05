/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

__attribute__((aligned (8))) int a[2048] = {};

void
f1 (void)
{
  for (int i = 0; i < 2048; i++)
    a[i]++;
}

/* { dg-final { scan-tree-dump-not "Vectorizing an unaligned access" "vect" } } */
/* { dg-final { scan-tree-dump-not "Alignment of access forced using peeling" "vect" } } */
