/* PR target/44606 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-skip-if "requires io" { freestanding } }  */

#include <stdio.h>

extern void abort (void);

 typedef struct _PixelPacket { 	unsigned char r, g, b; }
 PixelPacket;
#define ARRAYLEN(X) (sizeof(X)/sizeof(X[0]))
PixelPacket q[6];
#define COLS (ARRAYLEN(q) - 1)
PixelPacket p[2*COLS + 22];
#define Minify(POS, WEIGHT) do {	\
	total_r += (WEIGHT)*(p[POS].r);	\
	total_g += (WEIGHT)*(p[POS].g);	\
	total_b += (WEIGHT)*(p[POS].b);	\
} while (0)
unsigned long columns = COLS;
int main(void)
{
	static const unsigned char answers[COLS] = { 31, 32, 34, 35, 36 };
	unsigned long x;
	for (x = 0; x < sizeof(p)/sizeof(p[0]); x++) {
		p[x].b = (x + 34) | 1;
	}
	for (x = 0; x < columns; x++) {
		double total_r = 0, total_g = 0, total_b = 0;
		double saved_r = 0, saved_g = 0, saved_b = 0;
		Minify(2*x +  0,  3.0);
		Minify(2*x +  1,  7.0);
		Minify(2*x +  2,  7.0);
		saved_r = total_r;
		saved_g = total_g;
		Minify(2*x + 11, 15.0);
		Minify(2*x + 12,  7.0);
		Minify(2*x + 18,  7.0);
		Minify(2*x + 19, 15.0);
		Minify(2*x + 20, 15.0);
		Minify(2*x + 21,  7.0);
		q[x].r = (unsigned char)(total_r/128.0 + 0.5);
		q[x].g = (unsigned char)(total_g/128.0 + 0.5);
		q[x].b = (unsigned char)(total_b/128.0 + 0.5);
		fprintf(stderr, "r:%f g:%f b:%f\n", saved_r, saved_g, saved_b);
	}
	for (x = 0; x < COLS; x++) {
		if (answers[x] != q[x].b)
			abort();
	}
	return 0;
}
