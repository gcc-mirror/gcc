/* The precise output depends of sizeof(int) and sizeof(long long), so
   filter by target.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

/* Apparent uses of ^ for powers of 2.  */

short t2_16 = 2^16; /* { dg-warning "result of '2\\^16' is 18; did you mean '1 << 16' \\(65536\\)\\? \\\[-Wxor-used-as-pow\\\]" } */
int t2_17 = 2^17; /* { dg-warning "result of '2\\^17' is 19; did you mean '1 << 17' \\(131072\\)\\?" } */
int t2_30 = 2^30; /* { dg-warning "result of '2\\^30' is 28; did you mean '1 << 30' \\(1073741824\\)\\?" } */

/* Should be 1LL at 31 and above, due to overflow.  */
int t2_31 = 2^31; /* { dg-warning "result of '2\\^31' is 29; did you mean '1LL << 31'\\?" } */
int t2_32 = 2^32; /* { dg-warning "result of '2\\^32' is 34; did you mean '1LL << 32'\\?" } */

long t2_63 = 2^63; /* { dg-warning "result of '2\\^63' is 61; did you mean exponentiation\\?" } */
long t2_64 = 2^64; /* { dg-warning "result of '2\\^64' is 66; did you mean exponentiation\\?" } */

/* ...but don't warn when RHS is large enough.  */
long t2_65 = 2^65;
long t2_127 = 2^127;
long t2_128 = 2^128;
long t2_129 = 2^129;

/* Verify that -Wxor-used-as-pow happens before folding.  */
long t2_32_m1  = ((2^32)-1); /* { dg-warning "result of '2\\^32' is 34; did you mean '1LL << 32'\\?" } */


/* Apparent uses of ^ for powers of 10.  */

long t10_2 = 10^2; /* { dg-warning "result of '10\\^2' is 8; did you mean '1e2'\\?" } */
long t10_9 = 10^9; /* { dg-warning "result of '10\\^9' is 3; did you mean '1e9'\\?" } */
long t10_10 = 10^10; /* { dg-warning "result of '10\\^10' is 0; did you mean '1e10'\\?" } */
long t10_100 = 10^100; /* { dg-warning "result of '10\\^100' is 110; did you mean '1e100'\\?" } */

/* Don't warn on negatives.  */
long tm2_2 = -2^2;
long t2_m2 = 2^-2;
long tm10_10 = -10^10;
long t10_m10 = 10^-10;

/* If LHS is not 2 or 10, we shouldn't complain.  */
int t0_0 = 0 ^ 0;
int t6_7 = 6 ^ 7;

/* Floating point is already covered by type-checking.  */
float f10_10 = 10.f^10; /* { dg-error "invalid operands to binary \\^ \\(have 'float' and 'int'\\)" "" { target c } } */
/* { dg-error "invalid operands of types 'float' and 'int' to binary 'operator\\^'" "" { target c++ } .-1 } */

/* Don't complain if the LHS isn't literal decimal 2 or 10.  */
int t1p1_16 = (1 + 1) ^ 16;
int t5p5_6 = (5 + 5) ^ 6;
int h2_8 = 0x2 ^ 8;
int h10_3 = 0xa ^ 3;

/* Don't complain if the RHS isn't literal decimal.  */
int t2_x16 = 2^0x10;
int h10_x3 = 10 ^ 0x3;

/* Don't complain about uses in macros.  */
#define AMT (10^2)
int amt = AMT;
