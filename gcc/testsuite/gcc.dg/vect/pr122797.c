/* { dg-do run } */
/* { dg-additional-options "-O3" } */

int src_stride = 0;
int dst_stride = 0;

int main() {
    char src[12] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
    char dst[16];
    char *s = src;
    char *d = dst;
    for (int i = 0; i < 2; i++) {
	d[0] = s[0] + s[1] + s[2] + s[3] + s[4];
	d[1] = s[1] + s[2] + s[3] + s[4] + s[5];
	d[2] = s[2] + s[3] + s[4] + s[5] + s[6];
	d[3] = s[3] + s[4] + s[5] + s[6] + s[7];
	d[4] = s[4] + s[5] + s[6] + s[7] + s[8];
	d[5] = s[5] + s[6] + s[7] + s[8] + s[9];
	d[6] = s[6] + s[7] + s[8] + s[9] + s[10];
	d[7] = s[7] + s[8] + s[9] + s[10] + s[11];
	s += src_stride;
	d += dst_stride;
    }

    if (d[0] != 15)
      __builtin_abort ();
}
