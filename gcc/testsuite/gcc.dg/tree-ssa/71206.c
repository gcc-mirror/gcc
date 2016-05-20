/* { dg-do compile } */
/* { dg-options "-O" } */

int f(int d, unsigned b) {
    int i2 = b ^ 1;
    int i4 = d ^ 1;
    return i2 ^ i4;
}
