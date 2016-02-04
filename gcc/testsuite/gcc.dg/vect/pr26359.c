/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-dce6-details" } */

int a[256], b[256], c[256];

void
foo () {
  int i;

  for (i=0; i<256; i++){
    a[i] = b[i] + c[i];
  }
}

/* { dg-final { scan-tree-dump-times "Deleting : vect_" 0 "dce6" } } */
