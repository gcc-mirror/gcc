/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Make sure no ICE. */
unsigned char a;
int b(int c) {
  if (c >= 5000)
    return c / 5;
}
void d() { b(a - 5); }
int main() {}
