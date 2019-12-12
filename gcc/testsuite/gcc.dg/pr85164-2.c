/* { dg-options "-O2 -w" } */
int a;
long b;
void c() { b = -9223372036854775807L - 1 - a; }
