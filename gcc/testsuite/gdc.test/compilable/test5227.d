/*
REQUIRED_ARGS:
PERMUTE_ARGS:
TEST_OUTPUT:
---
log()
1.70475L
log2()
2.45943L
log10()
0.740363L
round()
6.00000L
floor()
5.00000F
5.00000
5.00000L
ceil()
6.00000F
6.00000
6.00000L
trunc()
5.00000L
expm1()
243.692L
exp2()
45.2548L
fmin()
-3.2L
fmax()
5.2L
copysign()
-2.5F
-2.5
-2.5L
pow()
9.88212F
9.88212
9.88212L
9.88212
fma()
-12.84L
---
*/

// https://issues.dlang.org/show_bug.cgi?id=5227

import std.math;

pragma(msg, "log()");
enum logf = log(5.5f); //pragma(msg, logf);
enum logd = log(5.5 ); //pragma(msg, logd);
enum logr = log(5.5L); pragma(msg, logr);

pragma(msg, "log2()");
enum log2f = log2(5.5f); //pragma(msg, log2f);
enum log2d = log2(5.5 ); //pragma(msg, log2d);
enum log2r = log2(5.5L); pragma(msg, log2r);

pragma(msg, "log10()");
enum log10f = log10(5.5f); //pragma(msg, log10f);
enum log10d = log10(5.5 ); //pragma(msg, log10d);
enum log10r = log10(5.5L); pragma(msg, log10r);

pragma(msg, "round()");
enum roundf = round(5.5f); //pragma(msg, roundf);
enum roundd = round(5.5 ); //pragma(msg, roundd);
enum roundr = round(5.5L); pragma(msg, roundr);

pragma(msg, "floor()");
enum floorf = floor(5.5f); pragma(msg, floorf);
enum floord = floor(5.5 ); pragma(msg, floord);
enum floorr = floor(5.5L); pragma(msg, floorr);

pragma(msg, "ceil()");
enum ceilf = ceil(5.5f); pragma(msg, ceilf);
enum ceild = ceil(5.5 ); pragma(msg, ceild);
enum ceilr = ceil(5.5L); pragma(msg, ceilr);

pragma(msg, "trunc()");
enum truncf = trunc(5.5f); //pragma(msg, truncf);
enum truncd = trunc(5.5 ); //pragma(msg, truncd);
enum truncr = trunc(5.5L); pragma(msg, truncr);

pragma(msg, "expm1()");
enum expm1f = expm1(5.5f); //pragma(msg, expm1f);
enum expm1d = expm1(5.5 ); //pragma(msg, expm1d);
enum expm1r = expm1(5.5L); pragma(msg, expm1r);

pragma(msg, "exp2()");
enum exp2f = exp2(5.5f); //pragma(msg, exp2f);
enum exp2d = exp2(5.5 ); //pragma(msg, exp2d);
enum exp2r = exp2(5.5L); pragma(msg, exp2r);



pragma(msg, "fmin()");
enum fminf = fmin(-3.2f, 5.2f); //pragma(msg, fminf);
enum fmind = fmin(-3.2 , 5.2 ); //pragma(msg, fmind);
enum fminr = fmin(-3.2L, 5.2L); pragma(msg, fminr);

pragma(msg, "fmax()");
enum fmaxf = fmax(-3.2f, 5.2f); //pragma(msg, fmaxf);
enum fmaxd = fmax(-3.2 , 5.2 ); //pragma(msg, fmaxd);
enum fmaxr = fmax(-3.2L, 5.2L); pragma(msg, fmaxr);

pragma(msg, "copysign()");
enum csf = copysign(2.5f, -3.0f); pragma(msg, csf); static assert(csf == -2.5);
enum csd = copysign(2.5 , -3.0 ); pragma(msg, csd); static assert(csd == -2.5);
enum csr = copysign(2.5L, -3.0L); pragma(msg, csr); static assert(csr == -2.5);

pragma(msg, "pow()");
enum powf = pow(2.5f, 2.5f); pragma(msg, powf);
enum powd = pow(2.5 , 2.5 ); pragma(msg, powd);
enum powr = pow(2.5L, 2.5L); pragma(msg, powr);
enum powctfe = 2.5 ^^ 2.5; pragma(msg, powctfe);


pragma(msg, "fma()");
enum fmaf = fma(-3.2f, 5.2f, 3.8f); //pragma(msg, fmaf);
enum fmad = fma(-3.2 , 5.2 , 3.8 ); //pragma(msg, fmad);
enum fmar = fma(-3.2L, 5.2L, 3.8L); pragma(msg, fmar);


