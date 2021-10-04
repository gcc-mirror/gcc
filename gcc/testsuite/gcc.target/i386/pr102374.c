/* PR target/102374 */

void calculate_sse(void) __attribute__ ((__target__ ("	no-avx, sse2   ")));
