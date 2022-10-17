/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2" } */

int printf(const char *, ...);
char a = 139, b;
int main() {
  char c = 173;
  b = a;
  while (c <= a || a < -117)
    c = printf("0\n");
  return 0;
}

/* { dg-final { scan-tree-dump-times  "if" 2 "dom2" } }  */
