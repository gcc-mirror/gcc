/* PR c/119651 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
int main() {
   int r = 0;
   while (r % 2 == 0) {
   }
   double r = (double) sizeof(int);  /* { dg-error "conflicting type" } */
  return 0;
}
