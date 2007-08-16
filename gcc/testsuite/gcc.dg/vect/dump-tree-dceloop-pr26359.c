/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a[256], b[256], c[256];

foo () {
  int i;

  for (i=0; i<256; i++){
    a[i] = b[i] + c[i];
  }
}

/* { dg-final { scan-tree-dump-times "Deleting : vect_" 0 "dceloop2" } } */
/* { dg-final { cleanup-tree-dump "dceloop\[1-2\]" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
