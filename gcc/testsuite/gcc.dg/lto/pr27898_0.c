/* PR c/27898 */
/* { dg-lto-do link } */

union u { struct { int i; }; };

extern int foo (union u *);

int main() { return 0; }
