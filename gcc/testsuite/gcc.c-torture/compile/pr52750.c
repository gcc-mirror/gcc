/* PR middle-end/52750 */

typedef signed char V __attribute__((vector_size (32)));

void
foo (V *x)
{
  V m = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
	  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  *x = __builtin_shuffle (*x, m);
}
