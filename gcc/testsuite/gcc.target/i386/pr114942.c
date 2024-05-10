/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-sra -fno-guess-branch-probability" } */

extern void a();
struct b {
  char c;
  char d;
} e;
int main() {
  struct b f = e;
  char i = 0;
 L1:
  if (!f.c)
    goto L2;
  if (e.c)
    a();
  else
    return 0;
  f.d = 0;
  i = 1 % ((1 & f.c) - 2);
 L2:
  f.c = ~(i & ~f.d);
  goto L1;
}
