// { dg-do compile { target d_runtime_has_std_library } }

//////////////////////////////////////////////////////
// std.math.exponential
import std.math.exponential;

enum test_exp = exp(1.0L);
enum test_expm1 = expm1(1.0L);
enum test_exp2 = exp2(1.0L);
enum test_log = log(1.0L);
enum test_log2 = log2(1.0L);
enum test_log10 = log10(1.0L);
enum test_pow = pow(1.0L, 1L);
enum test_powi = pow(1L, 1L);
enum test_powf = pow(1L, 1.0L);
enum test_powl = pow(1.0L, 1.0L);

//////////////////////////////////////////////////////
// std.math.operations
import std.math.operations;

enum test_fmin = fmin(1.0L, 2.0L);
enum test_fmax = fmax(1.0L, 2.0L);
enum test_fma = fma(1.0L, 2.0L, 3.0L);

//////////////////////////////////////////////////////
// std.math.rounding
import std.math.rounding;

enum test_round = round(12.34L);
enum test_floorf = floor(12.34f);
enum test_floor = floor(12.34);
enum test_floorl = floor(12.34L);
enum test_ceilf = ceil(12.34f);
enum test_ceil = ceil(12.34);
enum test_ceill = ceil(12.34L);
enum test_trunc = trunc(12.34L);

//////////////////////////////////////////////////////
// std.math.traits
import std.math.traits;

enum test_isNaN = isNaN(real.nan);
enum test_isInfinity = isInfinity(real.infinity);
enum test_isFinite = isFinite(1.0L);
enum test_copysign = copysign(1.0L, -1.0L);
enum test_copysigni = copysign(1L, -1.0L);

//////////////////////////////////////////////////////
// std.math.trigonometry
import std.math.trigonometry;

enum test_tan = tan(1.0L);
