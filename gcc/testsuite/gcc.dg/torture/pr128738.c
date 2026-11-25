/* { dg-do compile } */
int f(int x, int m) { return ((x >> 1) & 1) | ((x & m) != 0); }

