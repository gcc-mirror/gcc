/* { dg-do compile } */
/* { dg-options "-Wshift-count-negative -Wshift-count-overflow" } */

typedef unsigned int v1qi_t __attribute__((mode(QI), vector_size(1)));
typedef unsigned int v1hi_t __attribute__((mode(HI), vector_size(2)));
typedef unsigned int v1si_t __attribute__((mode(SI), vector_size(4)));

v1qi_t test1qi(v1qi_t x, int c) {
	switch(c) {
	case 0: return x << -1; /* { dg-warning "shift count is negative" } */
	case 1: return x << 7;
	case 2: return x << 8; /* { dg-warning "shift count >= width" } */
	case ~0: return x >> -1; /* { dg-warning "shift count is negative" } */
	case ~1: return x >> 7;
	case ~2: return x >> 8; /* { dg-warning "shift count >= width" } */
	}
	return c < 0 ? x >> -c : x << c;
}

v1hi_t test1hi(v1hi_t x, int c) {
	switch(c) {
	case 0: return x << -1; /* { dg-warning "shift count is negative" } */
	case 1: return x << 15;
	case 2: return x << 16; /* { dg-warning "shift count >= width" } */
	case ~0: return x >> -1; /* { dg-warning "shift count is negative" } */
	case ~1: return x >> 15;
	case ~2: return x >> 16; /* { dg-warning "shift count >= width" } */
	}
	return c < 0 ? x >> -c : x << c;
}

v1si_t test1si(v1si_t x, int c) {
	switch(c) {
	case 0: return x << -1; /* { dg-warning "shift count is negative" } */
	case 1: return x << 31;
	case 2: return x << 32; /* { dg-warning "shift count >= width" } */
	case ~0: return x >> -1; /* { dg-warning "shift count is negative" } */
	case ~1: return x >> 31;
	case ~2: return x >> 32; /* { dg-warning "shift count >= width" } */
	}
	return c < 0 ? x >> -c : x << c;
}
