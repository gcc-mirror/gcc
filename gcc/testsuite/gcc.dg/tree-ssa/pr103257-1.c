/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
void link_error(void);
unsigned b, c;
static short a(short e, short f) { return e * f; }
int main() {
  if (a(1  ^ ((0, 0) ^ 1 && b) <= b, c))
    link_error ();
  c = 0;
}
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
