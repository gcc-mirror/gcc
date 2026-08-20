/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O3" } */

/* The high 128 bits of a 128 x 128 -> 256 product has no widening
   multiply (no TI x TI -> OI) and no 256-bit expansion path.  forwprop
   folds the longhand to the canonical (uint256_t) a * (uint256_t) b
   >> 128 shape, and widening_mul re-synthesizes it from four
   64 x 64 -> 128 multiplies (mulq).  No __mulOI3 libcall.  */

__uint128_t
mulh_carry_128 (__uint128_t x, __uint128_t y)
{
    __uint128_t x_hi = x >> 64;
    __uint128_t x_lo = x & (__uint128_t) 0xFFFFFFFFFFFFFFFF;
    __uint128_t y_hi = y >> 64;
    __uint128_t y_lo = y & (__uint128_t) 0xFFFFFFFFFFFFFFFF;
    __uint128_t mulhilo = x_hi * y_lo;
    __uint128_t mullohi = x_lo * y_hi;
    __uint128_t cross_sum = mulhilo + mullohi;
    __uint128_t mullolo = x_lo * y_lo;
    __uint128_t shrlolo = mullolo >> 64;
    __uint128_t add_cross_sum = cross_sum + shrlolo;
    int carry = add_cross_sum < mulhilo;
    __uint128_t cond = ((__uint128_t) carry << 64) + x_hi * y_hi;
    __uint128_t add = cond + (add_cross_sum >> 64);

    return add;
}

/* { dg-final { scan-assembler-not "__multi3" } } */
/* { dg-final { scan-assembler-not "__mulOI3" } } */
/* { dg-final { scan-assembler-times "\tmul(q|x)" 4 } } */
