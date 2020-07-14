// { dg-do compile }
// { dg-options "-fdump-tree-original" }
import core.bitop;
import core.checkedint;
import core.math;
import core.stdc.stdarg;

//////////////////////////////////////////////////////
// core.bitop

// { dg-final { scan-tree-dump-not " bsf " "original" } }
int test_bsf(uint a) { return bsf(a); }
int test_bsf(ulong a) { return bsf(a); }
// { dg-final { scan-tree-dump-not " bsr " "original" } }
int test_bsr(uint a) { return bsr(a); }
int test_bsr(ulong a) { return bsr(a); }
// { dg-final { scan-tree-dump-not " bt " "original" } }
int test_bt(size_t *a, size_t b) { return bt(a, b); }
// { dg-final { scan-tree-dump-not " btc " "original" } }
int test_btc(size_t *a, size_t b) { return btc(a, b); }
// { dg-final { scan-tree-dump-not " btr " "original" } }
int test_btr(size_t *a, size_t b) { return btr(a, b); }
// { dg-final { scan-tree-dump-not " bts " "original" } }
int test_bts(size_t *a, size_t b) { return bts(a, b); }
// { dg-final { scan-tree-dump-not " bswap " "original" } }
uint test_bswap(uint a) { return bswap(a); }
ulong test_bswap(ulong a) { return bswap(a); }
// { dg-final { scan-tree-dump-not " popcnt " "original" } }
int test_popcnt(uint a) { return popcnt(a); }
int test_popcnt(ulong a) { return popcnt(a); }
// { dg-final { scan-tree-dump-not " volatileLoad " "original" } }
ubyte test_volatileLoad(ubyte *a) { return volatileLoad(a); }
ushort test_volatileLoad(ushort *a) { return volatileLoad(a); }
uint test_volatileLoad(uint *a) { return volatileLoad(a); }
ulong test_volatileLoad(ulong *a) { return volatileLoad(a); }
// { dg-final { scan-tree-dump-not " volatileStore " "original" } }
void test_volatileStore(ubyte *a, ubyte b) { return volatileStore(a, b); }
void test_volatileStore(ushort *a, ushort b) { return volatileStore(a, b); }
void test_volatileStore(uint *a, uint b) { return volatileStore(a, b); }
void test_volatileStore(ulong *a, ulong b) { return volatileStore(a, b); }

//////////////////////////////////////////////////////
// core.checkedint

// { dg-final { scan-tree-dump-not " adds " "original" } }
int test_adds(int a, int b, ref bool c) { return adds(a, b, c); }
long test_adds(long a, long b, ref bool c) { return adds(a, b, c); }
// { dg-final { scan-tree-dump-not " addu " "original" } }
uint test_addu(uint a, uint b, ref bool c) { return addu(a, b, c); }
ulong test_addu(ulong a, ulong b, ref bool c) { return addu(a, b, c); }
// { dg-final { scan-tree-dump-not " subs " "original" } }
int test_subs(int a, int b, ref bool c) { return subs(a, b, c); }
long test_subs(long a, long b, ref bool c) { return subs(a, b, c); }
// { dg-final { scan-tree-dump-not " subu " "original" } }
uint test_subu(uint a, uint b, ref bool c) { return subu(a, b, c); }
ulong test_subu(ulong a, ulong b, ref bool c) { return subu(a, b, c); }
// { dg-final { scan-tree-dump-not " negs " "original" } }
int test_negs(int a, ref bool b) { return negs(a, b); }
long test_negs(long a, ref bool b) { return negs(a, b); }
// { dg-final { scan-tree-dump-not " muls " "original" } }
int test_muls(int a, int b, ref bool c) { return muls(a, b, c); }
long test_muls(long a, long b, ref bool c) { return muls(a, b, c); }
// { dg-final { scan-tree-dump-not " mulu " "original" } }
uint test_mulu(uint a, uint b, ref bool c) { return mulu(a, b, c); }
ulong test_mulu(ulong a, uint b, ref bool c) { return mulu(a, b, c); }
ulong test_mulu(ulong a, ulong b, ref bool c) { return mulu(a, b, c); }

//////////////////////////////////////////////////////
// core.math

// { dg-final { scan-tree-dump-not " cos " "original" } }
float test_cos(float a) { return cos(a); }
double test_cos(double a) { return cos(a); }
real test_cos(real a) { return cos(a); }
// { dg-final { scan-tree-dump-not " sin " "original" } }
float test_sin(float a) { return sin(a); }
double test_sin(double a) { return sin(a); }
real test_sin(real a) { return sin(a); }
// { dg-final { scan-tree-dump-not " rndtol " "original" } }
long test_rndtol(float a) { return rndtol(a); }
long test_rndtol(double a) { return rndtol(a); }
long test_rndtol(real a) { return rndtol(a); }
// { dg-final { scan-tree-dump-not " sqrt " "original" } }
float test_sqrt(float a) { return sqrt(a); }
double test_sqrt(double a) { return sqrt(a); }
real test_sqrt(real a) { return sqrt(a); }
// { dg-final { scan-tree-dump-not " ldexp " "original" } }
float test_ldexp(float a, int b) { return ldexp(a, b); }
double test_ldexp(double a, int b) { return ldexp(a, b); }
real test_ldexp(real a, int b) { return ldexp(a, b); }
// { dg-final { scan-tree-dump-not " fabs " "original" } }
float test_fabs(float a) { return fabs(a); }
double test_fabs(double a) { return fabs(a); }
real test_fabs(real a) { return fabs(a); }
// { dg-final { scan-tree-dump-not " rint " "original" } }
float test_rint(float a) { return rint(a); }
double test_rint(double a) { return rint(a); }
real test_rint(real a) { return rint(a); }
// { dg-final { scan-tree-dump-not " toPrec " "original" } }
float test_toPrec(float a) { return toPrec!float(a); }
float test_toPrec(double a) { return toPrec!float(a); }
float test_toPrec(real a) { return toPrec!float(a); }
double test_toPrec(float a) { return toPrec!double(a); }
double test_toPrec(double a) { return toPrec!double(a); }
double test_toPrec(real a) { return toPrec!double(a); }
real test_toPrec(float a) { return toPrec!real(a); }
real test_toPrec(double a) { return toPrec!real(a); }
real test_toPrec(real a) { return toPrec!real(a); }

//////////////////////////////////////////////////////
// core.stdc.stdarg

// { dg-final { scan-tree-dump-not " va_arg " "original" } }
void test_va_arg(...) { int a; return va_arg!int(_argptr, a); }
int test_va_arg(...) { return va_arg!int(_argptr); }
// { dg-final { scan-tree-dump-not " va_start " "original" } }
void test_va_start(int a, ...) { return va_start(_argptr, a); }
