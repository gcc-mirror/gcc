/* { dg-do compile } */
/* { dg-additional-options "-w" } */

extern struct {
  unsigned char a;
  unsigned char b;
  unsigned char c;
  unsigned char d;
} g[];
void main() { g[0].b = (g[0].b & g[4].b) * g[2305843009213693952ULL].c; }
