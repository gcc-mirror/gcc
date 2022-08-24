/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f1(int x) { return ((x >> 8) & 6) != 0; }
int f2(int x) { return ((x << 2) & 24) != 0; }
int f3(unsigned x) { return ((x << 2) & 15) != 0; }
int f4(unsigned x) { return ((x >> 2) & 14) != 0; }

int fifth (int c)
{
    int a = (c >> 8) & 7;

    if (a >= 2) {
	return 1;
    } else {
	return 0;
    }
}
/* { dg-final { scan-tree-dump-not " << " "optimized" } } */
/* { dg-final { scan-tree-dump-not " >> " "optimized" } } */

