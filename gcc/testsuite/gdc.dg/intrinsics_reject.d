// { dg-do compile }
import core.bitop;
import core.math;
import core.volatile;
import core.stdc.stdarg;

//////////////////////////////////////////////////////
// core.bitop

int function(uint) test_bsf() { return &bsf; }
int function(ulong) test_bsfl() { return &bsf; }
int function(uint) test_bsr() { return &bsr; }
int function(ulong) test_bsrl() { return &bsr; }
auto test_bt() { return &bt; }
// { dg-error "intrinsic function .btc. must be directly called" "" { target *-*-* } .+1 }
auto test_btc() { return &btc; }
// { dg-error "intrinsic function .btr. must be directly called" "" { target *-*-* } .+1 }
auto test_btr() { return &btr; }
// { dg-error "intrinsic function .bts. must be directly called" "" { target *-*-* } .+1 }
auto test_bts() { return &bts; }
// { dg-error "intrinsic function .byteswap. must be directly called" "" { target *-*-* } .+1 }
ushort function(ushort) test_byteswap() { return &byteswap; }
// { dg-error "intrinsic function .bswap. must be directly called" "" { target *-*-* } .+1 }
uint function(uint) test_bswap() { return &bswap; }
// { dg-error "intrinsic function .bswap. must be directly called" "" { target *-*-* } .+1 }
ulong function(ulong) test_bswapl() { return &bswap; }
int function(uint) test_popcnt() { return &popcnt; }
int function(ulong) test_popcntl() { return &popcnt; }
auto test_rol() { return &rol!ubyte; }
auto test_rol(uint a) { return &rol!(1, uint); }
auto test_ror(ushort a, uint b) { return &ror!ushort; }
auto test_ror(ulong a) { return &ror!(1, ulong); }

//////////////////////////////////////////////////////
// core.math

float function(float) test_cosf() { return &cos; }
double function(double) test_cos() { return &cos; }
real function(real) test_cosl() { return &cos; }
float function(float) test_sinf() { return &sin; }
double function(double) test_sin() { return &sin; }
real function(real) test_sinl() { return &sin; }
long function(float) test_rndtolf() { return &rndtol; }
long function(double) test_rndtol() { return &rndtol; }
long function(real) test_rndtoll() { return &rndtol; }
float function(float) test_sqrtf() { return &sqrt; }
double function(double) test_sqrt() { return &sqrt; }
real function(real) test_sqrtl() { return &sqrt; }
float function(float, int) test_ldexpf() { return &ldexp; }
double function(double, int) test_ldexp() { return &ldexp; }
real function(real, int) test_ldexpl() { return &ldexp; }
float function(float) test_fabsf() { return &fabs; }
double function(double) test_fabs() { return &fabs; }
real function(real) test_fabsl() { return &fabs; }
float function(float) test_rintf() { return &rint; }
double function(double) test_rint() { return &rint; }
real function(real) test_rintl() { return &rint; }

//////////////////////////////////////////////////////
// core.volatile

// { dg-error "intrinsic function .volatileLoad. must be directly called" "" { target *-*-* } .+1 }
ubyte function(ubyte*) test_volatileLoad8() { return &volatileLoad; }
// { dg-error "intrinsic function .volatileLoad. must be directly called" "" { target *-*-* } .+1 }
ushort function(ushort*) test_volatileLoad16() { return &volatileLoad; }
// { dg-error "intrinsic function .volatileLoad. must be directly called" "" { target *-*-* } .+1 }
uint function(uint*) test_volatileLoad32() { return &volatileLoad; }
// { dg-error "intrinsic function .volatileLoad. must be directly called" "" { target *-*-* } .+1 }
ulong function(ulong*) test_volatileLoad64() { return &volatileLoad; }
// { dg-error "intrinsic function .volatileStore. must be directly called" "" { target *-*-* } .+1 }
void function(ubyte*, ubyte) test_volatileStore8() { return &volatileStore; }
// { dg-error "intrinsic function .volatileStore. must be directly called" "" { target *-*-* } .+1 }
void function(ushort*, ushort) test_volatileStore16() { return &volatileStore; }
// { dg-error "intrinsic function .volatileStore. must be directly called" "" { target *-*-* } .+1 }
void function(uint*, uint) test_volatileStore32() { return &volatileStore; }
// { dg-error "intrinsic function .volatileStore. must be directly called" "" { target *-*-* } .+1 }
void function(ulong*, ulong) test_volatileStore64() { return &volatileStore; }

//////////////////////////////////////////////////////
// core.stdc.stdarg

// { dg-error "intrinsic function .va_start. must be directly called" "" { target *-*-* } .+1 }
auto test_va_start() { return &va_start!int; }
// { dg-error "built-in function .__builtin_va_end. must be directly called" "" { target *-*-* } .+1 }
auto test_va_end() { return &va_end; }
// { dg-error "built-in function .__builtin_va_copy. must be directly called" "" { target *-*-* } .+1 }
auto test_va_copy() { return &va_copy; }
