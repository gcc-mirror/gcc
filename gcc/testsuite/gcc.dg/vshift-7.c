/* { dg-do compile } */
/* { dg-options "-O1 -Wshift-count-negative -Wshift-count-overflow" } */

typedef unsigned int v1qi_t __attribute__((mode(QI), vector_size(1)));
typedef unsigned int v1hi_t __attribute__((mode(HI), vector_size(2)));
typedef unsigned int v1si_t __attribute__((mode(SI), vector_size(4)));

static const signed shift_neg = -1;
static const unsigned shift_qi = 8;
static const unsigned shift_hi = 16;
static const unsigned shift_si = 32;

v1qi_t test1qi(v1qi_t x, int c) {
	switch(c) {
	case 0: return x << shift_neg; /* { dg-warning "shift count is negative" } */
	case 1: return x << (shift_qi - 1);
	case 2: return x << shift_qi; /* { dg-warning "shift count >= width" } */
	case ~0: return x >> shift_neg; /* { dg-warning "shift count is negative" } */
	case ~1: return x >> (shift_qi - 1);
	case ~2: return x >> shift_qi; /* { dg-warning "shift count >= width" } */
	}
	return c < 0 ? x >> -c : x << c;
}

v1hi_t test1hi(v1hi_t x, int c) {
	switch(c) {
	case 0: return x << shift_neg; /* { dg-warning "shift count is negative" } */
	case 1: return x << (shift_hi - 1);
	case 2: return x << shift_hi; /* { dg-warning "shift count >= width" } */
	case ~0: return x >> shift_neg; /* { dg-warning "shift count is negative" } */
	case ~1: return x >> (shift_hi - 1);
	case ~2: return x >> shift_hi; /* { dg-warning "shift count >= width" } */
	}
	return c < 0 ? x >> -c : x << c;
}

v1si_t test1si(v1si_t x, int c) {
	switch(c) {
	case 0: return x << shift_neg; /* { dg-warning "shift count is negative" } */
	case 1: return x << (shift_si - 1);
	case 2: return x << shift_si; /* { dg-warning "shift count >= width" } */
	case ~0: return x >> shift_neg; /* { dg-warning "shift count is negative" } */
	case ~1: return x >> (shift_si - 1);
	case ~2: return x >> shift_si; /* { dg-warning "shift count >= width" } */
	}
	return c < 0 ? x >> -c : x << c;
}
