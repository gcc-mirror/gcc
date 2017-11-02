/* { dg-do compile } */
/* { dg-options "-Os -w" } */

int a[1][1];
int main() { int *b[] = {a, a[1820408606019012862278468], a, a, a}; }
