/* { dg-do compile } */
/* { dg-options "-O3 -fno-inline -fno-tree-vrp -fno-ipa-sra -fno-tree-dce -fno-tree-ch" } */

int a, c, e, f, g, h[1], i;
static int j(int b) { return 0; }
static void k(int d) {}
int main()
{
  if (h[0])
    while (1) {
	k(f && j(i && (h[g] = e)));
	while (a)
	  c ^= 1;
    }
  return 0;
}
