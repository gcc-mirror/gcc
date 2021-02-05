// { dg-do compile }
// { dg-options "-fdump-tree-original" }
import core.bitop;
import core.checkedint;
import core.math;
import core.volatile;
import core.stdc.stdarg;

//////////////////////////////////////////////////////
// core.bitop

// { dg-final { scan-tree-dump " __builtin_ctz " "original" } }
int test_bsf(uint a) { return bsf(a); }
// { dg-final { scan-tree-dump " __builtin_ctz(l|ll) " "original" } }
int test_bsf(ulong a) { return bsf(a); }
// { dg-final { scan-tree-dump " __builtin_clz " "original" } }
int test_bsr(uint a) { return bsr(a); }
// { dg-final { scan-tree-dump " __builtin_clz(l|ll) " "original" } }
int test_bsr(ulong a) { return bsr(a); }
// { dg-final { scan-tree-dump-not " <retval> = bt " "original" } }
int test_bt(size_t *a, size_t b) { return bt(a, b); }
// { dg-final { scan-tree-dump-not " <retval> = btc " "original" } }
int test_btc(size_t *a, size_t b) { return btc(a, b); }
// { dg-final { scan-tree-dump-not " <retval> = btr " "original" } }
int test_btr(size_t *a, size_t b) { return btr(a, b); }
// { dg-final { scan-tree-dump-not " <retval> = bts " "original" } }
int test_bts(size_t *a, size_t b) { return bts(a, b); }
// { dg-final { scan-tree-dump " __builtin_bswap16 " "original" } }
ushort test_byteswap(ushort a) { return byteswap(a); }
// { dg-final { scan-tree-dump " __builtin_bswap32 " "original" } }
uint test_bswap(uint a) { return bswap(a); }
// { dg-final { scan-tree-dump " __builtin_bswap64 " "original" } }
ulong test_bswap(ulong a) { return bswap(a); }
// { dg-final { scan-tree-dump " __builtin_popcount " "original" } }
int test_popcnt(uint a) { return popcnt(a); }
// { dg-final { scan-tree-dump " __builtin_popcount(l|ll) " "original" } }
int test_popcnt(ulong a) { return popcnt(a); }
// { dg-final { scan-tree-dump "\\(volatile ubyte \\*\\) a;" "original" } }
ubyte test_volatileLoad(ubyte *a) { return volatileLoad(a); }
// { dg-final { scan-tree-dump "\\(volatile ushort \\*\\) a;" "original" } }
ushort test_volatileLoad(ushort *a) { return volatileLoad(a); }
// { dg-final { scan-tree-dump "\\(volatile uint \\*\\) a;" "original" } }
uint test_volatileLoad(uint *a) { return volatileLoad(a); }
// { dg-final { scan-tree-dump "\\(volatile ulong \\*\\) a;" "original" } }
ulong test_volatileLoad(ulong *a) { return volatileLoad(a); }
// { dg-final { scan-tree-dump "\\(volatile ubyte \\*\\) a = b" "original" } }
void test_volatileStore(ubyte *a, ubyte b) { return volatileStore(a, b); }
// { dg-final { scan-tree-dump "\\(volatile ushort \\*\\) a = b" "original" } }
void test_volatileStore(ushort *a, ushort b) { return volatileStore(a, b); }
// { dg-final { scan-tree-dump "\\(volatile uint \\*\\) a = b" "original" } }
void test_volatileStore(uint *a, uint b) { return volatileStore(a, b); }
// { dg-final { scan-tree-dump "\\(volatile ulong \\*\\) a = b" "original" } }
void test_volatileStore(ulong *a, ulong b) { return volatileStore(a, b); }
// { dg-final { scan-tree-dump " a r<< b;" "original" } }
ubyte test_rol(ubyte a, uint b) { return rol!ubyte(a, b); }
// { dg-final { scan-tree-dump " a r>> 31;" "original" } }
uint test_rol(uint a) { return rol!(1, uint)(a); }
// { dg-final { scan-tree-dump " a r>> b;" "original" } }
ushort test_ror(ushort a, uint b) { return ror!ushort(a, b); }
// { dg-final { scan-tree-dump " a r>> 1;" "original" } }
ulong test_ror(ulong a) { return ror!(1, ulong)(a); }

//////////////////////////////////////////////////////
// core.checkedint

// { dg-final { scan-tree-dump-not " <retval> = adds " "original" } }
int test_adds(int a, int b, ref bool c) { return adds(a, b, c); }
long test_adds(long a, long b, ref bool c) { return adds(a, b, c); }
// { dg-final { scan-tree-dump-not " <retval> = addu " "original" } }
uint test_addu(uint a, uint b, ref bool c) { return addu(a, b, c); }
ulong test_addu(ulong a, ulong b, ref bool c) { return addu(a, b, c); }
// { dg-final { scan-tree-dump-not " <retval> = subs " "original" } }
int test_subs(int a, int b, ref bool c) { return subs(a, b, c); }
long test_subs(long a, long b, ref bool c) { return subs(a, b, c); }
// { dg-final { scan-tree-dump-not " <retval> = subu " "original" } }
uint test_subu(uint a, uint b, ref bool c) { return subu(a, b, c); }
ulong test_subu(ulong a, ulong b, ref bool c) { return subu(a, b, c); }
// { dg-final { scan-tree-dump-not " <retval> = negs " "original" } }
int test_negs(int a, ref bool b) { return negs(a, b); }
long test_negs(long a, ref bool b) { return negs(a, b); }
// { dg-final { scan-tree-dump-not " <retval> = muls " "original" } }
int test_muls(int a, int b, ref bool c) { return muls(a, b, c); }
long test_muls(long a, long b, ref bool c) { return muls(a, b, c); }
// { dg-final { scan-tree-dump-not " <retval> = mulu " "original" } }
uint test_mulu(uint a, uint b, ref bool c) { return mulu(a, b, c); }
ulong test_mulu(ulong a, uint b, ref bool c) { return mulu(a, b, c); }
ulong test_mulu(ulong a, ulong b, ref bool c) { return mulu(a, b, c); }

//////////////////////////////////////////////////////
// core.math

// { dg-final { scan-tree-dump " __builtin_cosf " "original" } }
float test_cos(float a) { return cos(a); }
// { dg-final { scan-tree-dump " __builtin_cos " "original" } }
double test_cos(double a) { return cos(a); }
// { dg-final { scan-tree-dump " __builtin_cosl " "original" } }
real test_cos(real a) { return cos(a); }
// { dg-final { scan-tree-dump " __builtin_sinf " "original" } }
float test_sin(float a) { return sin(a); }
// { dg-final { scan-tree-dump " __builtin_sin " "original" } }
double test_sin(double a) { return sin(a); }
// { dg-final { scan-tree-dump " __builtin_sinl " "original" } }
real test_sin(real a) { return sin(a); }
// { dg-final { scan-tree-dump " __builtin_llroundf " "original" } }
long test_rndtol(float a) { return rndtol(a); }
// { dg-final { scan-tree-dump " __builtin_llround " "original" } }
long test_rndtol(double a) { return rndtol(a); }
// { dg-final { scan-tree-dump " __builtin_llroundl " "original" } }
long test_rndtol(real a) { return rndtol(a); }
// { dg-final { scan-tree-dump " __builtin_sqrtf " "original" } }
float test_sqrt(float a) { return sqrt(a); }
// { dg-final { scan-tree-dump " __builtin_sqrt " "original" } }
double test_sqrt(double a) { return sqrt(a); }
// { dg-final { scan-tree-dump " __builtin_sqrtl " "original" } }
real test_sqrt(real a) { return sqrt(a); }
// { dg-final { scan-tree-dump " __builtin_ldexpf " "original" } }
float test_ldexp(float a, int b) { return ldexp(a, b); }
// { dg-final { scan-tree-dump " __builtin_ldexp " "original" } }
double test_ldexp(double a, int b) { return ldexp(a, b); }
// { dg-final { scan-tree-dump " __builtin_ldexpl " "original" } }
real test_ldexp(real a, int b) { return ldexp(a, b); }
// { dg-final { scan-tree-dump-not " <retval> = fabs " "original" } }
float test_fabs(float a) { return fabs(a); }
double test_fabs(double a) { return fabs(a); }
real test_fabs(real a) { return fabs(a); }
// { dg-final { scan-tree-dump " __builtin_rintf " "original" } }
float test_rint(float a) { return rint(a); }
// { dg-final { scan-tree-dump " __builtin_rint " "original" } }
double test_rint(double a) { return rint(a); }
// { dg-final { scan-tree-dump " __builtin_rintl " "original" } }
real test_rint(real a) { return rint(a); }
// { dg-final { scan-tree-dump-not " <retval> = toPrec " "original" } }
float test_toPrecf(float a) { return toPrec!float(a); }
float test_toPrecf(double a) { return toPrec!float(a); }
float test_toPrecf(real a) { return toPrec!float(a); }
double test_toPrec(float a) { return toPrec!double(a); }
double test_toPrec(double a) { return toPrec!double(a); }
double test_toPrec(real a) { return toPrec!double(a); }
real test_toPrecl(float a) { return toPrec!real(a); }
real test_toPrecl(double a) { return toPrec!real(a); }
real test_toPrecl(real a) { return toPrec!real(a); }

//////////////////////////////////////////////////////
// core.stdc.stdarg

// { dg-final { scan-tree-dump-not " va_arg " "original" } }
void test_va_argc(...) { int a; return va_arg!int(_argptr, a); }
int test_va_arg(...) { return va_arg!int(_argptr); }
// { dg-final { scan-tree-dump-not " va_start " "original" } }
void test_va_start(int a, ...) { return va_start(_argptr, a); }
