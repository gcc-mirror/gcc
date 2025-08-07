/* { dg-do compile { target bitint } } */
/* { dg-options "-march=z9-109 -fdump-rtl-expand" } */

/* Verify calling convention. */

/* { dg-final { scan-rtl-dump-times "zero_extend:DI.*reg:QI" 1 "expand" } } */
void bitint5_zero_extend (unsigned _BitInt(5) x);
void bitint5_zero_extend_call (unsigned _BitInt(5) x) { bitint5_zero_extend (x + 1); }

/* { dg-final { scan-rtl-dump-times "sign_extend:DI.*reg:QI" 1 "expand" } } */
void bitint5_sign_extend (_BitInt(5) x);
void bitint5_sign_extend_call (_BitInt(5) x) { bitint5_sign_extend (x + 1); }

/* { dg-final { scan-rtl-dump-times "zero_extend:DI.*reg:HI" 1 "expand" } } */
void bitint9_zero_extend (unsigned _BitInt(9) x);
void bitint9_zero_extend_call (unsigned _BitInt(9) x) { bitint9_zero_extend (x + 1); }

/* { dg-final { scan-rtl-dump-times "sign_extend:DI.*reg:HI" 1 "expand" } } */
void bitint9_sign_extend (_BitInt(9) x);
void bitint9_sign_extend_call (_BitInt(9) x) { bitint9_sign_extend (x + 1); }

/* { dg-final { scan-rtl-dump-times "zero_extend:DI.*reg:SI" 1 "expand" } } */
void bitint17_zero_extend (unsigned _BitInt(17) x);
void bitint17_zero_extend_call (unsigned _BitInt(17) x) { bitint17_zero_extend (x + 1); }

/* { dg-final { scan-rtl-dump-times "sign_extend:DI.*reg:SI" 1 "expand" } } */
void bitint17_sign_extend (_BitInt(17) x);
void bitint17_sign_extend_call (_BitInt(17) x) { bitint17_sign_extend (x + 1); }
