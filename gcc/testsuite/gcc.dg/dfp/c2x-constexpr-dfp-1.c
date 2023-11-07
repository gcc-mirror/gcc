/* Test C23 constexpr.  Valid code, compilation tests, DFP.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

constexpr _Decimal32 v1 = __DEC32_MIN__;
constexpr _Decimal32 v2 = __DEC32_SUBNORMAL_MIN__;
constexpr _Decimal32 v3 = -__DEC32_MAX__;
constexpr _Decimal64 v4 = __DEC32_MIN__;
constexpr _Decimal64 v5 = __DEC32_SUBNORMAL_MIN__;
constexpr _Decimal64 v6 = -__DEC32_MAX__;
constexpr _Decimal64 v7 = __DEC64_MIN__;
constexpr _Decimal64 v8 = __DEC64_SUBNORMAL_MIN__;
constexpr _Decimal64 v9 = -__DEC64_MAX__;
constexpr _Decimal128 v10 = __DEC32_MIN__;
constexpr _Decimal128 v11 = __DEC32_SUBNORMAL_MIN__;
constexpr _Decimal128 v12 = -__DEC32_MAX__;
constexpr _Decimal128 v13 = __DEC64_MIN__;
constexpr _Decimal128 v14 = __DEC64_SUBNORMAL_MIN__;
constexpr _Decimal128 v15 = -__DEC64_MAX__;
constexpr _Decimal128 v16 = __DEC128_MIN__;
constexpr _Decimal128 v17 = __DEC128_SUBNORMAL_MIN__;
constexpr _Decimal128 v18 = -__DEC128_MAX__;
constexpr _Decimal32 v19 = 1234567L;
constexpr _Decimal32 v20 = -123456700000LL;
constexpr _Decimal64 v21 = 1234567890123456LL;
constexpr _Decimal64 v22 = -123456789012345600LL;
constexpr _Decimal128 v23 = (unsigned long long) -1;
constexpr _Decimal32 v24 = 1e-101DL;
constexpr _Decimal64 v25 = 1e-398DL;
constexpr _Decimal32 v26 = __builtin_infd128 ();
constexpr _Decimal128 v27 = __builtin_infd32 ();
constexpr _Decimal64 v28 = __builtin_nand128 ("");
constexpr _Decimal128 v29 = __builtin_nand32 ("");
constexpr _Decimal32 v30 = __builtin_nansd32 ("");
constexpr _Decimal64 v31 = __builtin_nansd64 ("");
constexpr _Decimal128 v32 = __builtin_nansd128 ("");
constexpr _Decimal32 v33 = {};
constexpr _Decimal64 v34 = {};
constexpr _Decimal128 v35 = {};
constexpr _Decimal32 v36 = 0.0;
constexpr _Decimal32 v37 = 0.0009765625;
constexpr _Decimal64 v38 = 6.103515625e-05;
constexpr _Decimal32 v39 = __builtin_inf ();
constexpr _Decimal32 v40 = __builtin_nan ("");

void
f0 ()
{
  (constexpr _Decimal32) { __DEC32_MIN__ };
  (constexpr _Decimal32) { __DEC32_SUBNORMAL_MIN__ };
  (constexpr _Decimal32) { -__DEC32_MAX__ };
  (constexpr _Decimal64) { __DEC32_MIN__ };
  (constexpr _Decimal64) { __DEC32_SUBNORMAL_MIN__ };
  (constexpr _Decimal64) { -__DEC32_MAX__ };
  (constexpr _Decimal64) { __DEC64_MIN__ };
  (constexpr _Decimal64) { __DEC64_SUBNORMAL_MIN__ };
  (constexpr _Decimal64) { -__DEC64_MAX__ };
  (constexpr _Decimal128) { __DEC32_MIN__ };
  (constexpr _Decimal128) { __DEC32_SUBNORMAL_MIN__ };
  (constexpr _Decimal128) { -__DEC32_MAX__ };
  (constexpr _Decimal128) { __DEC64_MIN__ };
  (constexpr _Decimal128) { __DEC64_SUBNORMAL_MIN__ };
  (constexpr _Decimal128) { -__DEC64_MAX__ };
  (constexpr _Decimal128) { __DEC128_MIN__ };
  (constexpr _Decimal128) { __DEC128_SUBNORMAL_MIN__ };
  (constexpr _Decimal128) { -__DEC128_MAX__ };
  (constexpr _Decimal32) { 1234567L };
  (constexpr _Decimal32) { -123456700000LL };
  (constexpr _Decimal64) { 1234567890123456LL };
  (constexpr _Decimal64) { -123456789012345600LL };
  (constexpr _Decimal128) { (unsigned long long) -1 };
  (constexpr _Decimal32) { 1e-101DL };
  (constexpr _Decimal64) { 1e-398DL };
  (constexpr _Decimal32) { __builtin_infd128 () };
  (constexpr _Decimal128) { __builtin_infd32 () };
  (constexpr _Decimal64) { __builtin_nand128 ("") };
  (constexpr _Decimal128) { __builtin_nand32 ("") };
  (constexpr _Decimal32) { __builtin_nansd32 ("") };
  (constexpr _Decimal64) { __builtin_nansd64 ("") };
  (constexpr _Decimal128) { __builtin_nansd128 ("") };
  (constexpr _Decimal32) {};
  (constexpr _Decimal64) {};
  (constexpr _Decimal128) {};
  (constexpr _Decimal32) { 0.0 };
  (constexpr _Decimal32) { 0.0009765625 };
  (constexpr _Decimal64) { 6.103515625e-05 };
  (constexpr _Decimal32) { __builtin_inf () };
  (constexpr _Decimal32) { __builtin_nan ("") };
}
