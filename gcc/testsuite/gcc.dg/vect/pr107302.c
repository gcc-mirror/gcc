/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-pre" } */

int a[2000];
int s292_im1;

void
s292() {
  for (int i = 0; i < 2000; i++) {
    a[i] = s292_im1;
    s292_im1 = i;
  }
}
