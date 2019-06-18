// PERMUTE_ARGS:
// REQUIRED_ARGS: -O
// POST_SCRIPT: compilable/extra-files/objdump-postscript.sh
// only testing on SYSV-ABI, but backend code is identical across platforms
// DISABLED: win32 win64 osx linux32 freebsd32 freebsd64

bool test_ltz(ubyte x) { return x <  0; }
bool test_lez(ubyte x) { return x <= 0; }
bool test_eqz(ubyte x) { return x == 0; }
bool test_nez(ubyte x) { return x != 0; }
bool test_gez(ubyte x) { return x >= 0; }
bool test_gtz(ubyte x) { return x >  0; }

bool test_ltz(byte x) { return x <  0; }
bool test_lez(byte x) { return x <= 0; }
bool test_eqz(byte x) { return x == 0; }
bool test_nez(byte x) { return x != 0; }
bool test_gez(byte x) { return x >= 0; }
bool test_gtz(byte x) { return x >  0; }

bool test_ltz(ushort x) { return x <  0; }
bool test_lez(ushort x) { return x <= 0; }
bool test_eqz(ushort x) { return x == 0; }
bool test_nez(ushort x) { return x != 0; }
bool test_gez(ushort x) { return x >= 0; }
bool test_gtz(ushort x) { return x >  0; }

bool test_ltz(short x) { return x <  0; }
bool test_lez(short x) { return x <= 0; }
bool test_eqz(short x) { return x == 0; }
bool test_nez(short x) { return x != 0; }
bool test_gez(short x) { return x >= 0; }
bool test_gtz(short x) { return x >  0; }

bool test_ltz(uint x) { return x <  0; }
bool test_lez(uint x) { return x <= 0; }
bool test_eqz(uint x) { return x == 0; }
bool test_nez(uint x) { return x != 0; }
bool test_gez(uint x) { return x >= 0; }
bool test_gtz(uint x) { return x >  0; }

bool test_ltz(int x) { return x <  0; }
bool test_lez(int x) { return x <= 0; }
bool test_eqz(int x) { return x == 0; }
bool test_nez(int x) { return x != 0; }
bool test_gez(int x) { return x >= 0; }
bool test_gtz(int x) { return x >  0; }

bool test_ltz(ulong x) { return x <  0; }
bool test_lez(ulong x) { return x <= 0; }
bool test_eqz(ulong x) { return x == 0; }
bool test_nez(ulong x) { return x != 0; }
bool test_gez(ulong x) { return x >= 0; }
bool test_gtz(ulong x) { return x >  0; }

bool test_ltz(long x) { return x <  0; }
bool test_lez(long x) { return x <= 0; }
bool test_eqz(long x) { return x == 0; }
bool test_nez(long x) { return x != 0; }
bool test_gez(long x) { return x >= 0; }
bool test_gtz(long x) { return x >  0; }

bool test_ltz(float x) { return x <  0; }
bool test_lez(float x) { return x <= 0; }
bool test_eqz(float x) { return x == 0; }
bool test_nez(float x) { return x != 0; }
bool test_gez(float x) { return x >= 0; }
bool test_gtz(float x) { return x >  0; }

bool test_ltz(double x) { return x <  0; }
bool test_lez(double x) { return x <= 0; }
bool test_eqz(double x) { return x == 0; }
bool test_nez(double x) { return x != 0; }
bool test_gez(double x) { return x >= 0; }
bool test_gtz(double x) { return x >  0; }

/* ----------------------------------- */

bool test_lt(ubyte x, ubyte y) { return x <  y; }
bool test_le(ubyte x, ubyte y) { return x <= y; }
bool test_eq(ubyte x, ubyte y) { return x == y; }
bool test_ne(ubyte x, ubyte y) { return x != y; }
bool test_ge(ubyte x, ubyte y) { return x >= y; }
bool test_gt(ubyte x, ubyte y) { return x >  y; }

bool test_lt(byte x, byte y) { return x <  y; }
bool test_le(byte x, byte y) { return x <= y; }
bool test_eq(byte x, byte y) { return x == y; }
bool test_ne(byte x, byte y) { return x != y; }
bool test_ge(byte x, byte y) { return x >= y; }
bool test_gt(byte x, byte y) { return x >  y; }

bool test_lt(ushort x, ushort y) { return x <  y; }
bool test_le(ushort x, ushort y) { return x <= y; }
bool test_eq(ushort x, ushort y) { return x == y; }
bool test_ne(ushort x, ushort y) { return x != y; }
bool test_ge(ushort x, ushort y) { return x >= y; }
bool test_gt(ushort x, ushort y) { return x >  y; }

bool test_lt(short x, short y) { return x <  y; }
bool test_le(short x, short y) { return x <= y; }
bool test_eq(short x, short y) { return x == y; }
bool test_ne(short x, short y) { return x != y; }
bool test_ge(short x, short y) { return x >= y; }
bool test_gt(short x, short y) { return x >  y; }

bool test_lt(uint x, uint y) { return x <  y; }
bool test_le(uint x, uint y) { return x <= y; }
bool test_eq(uint x, uint y) { return x == y; }
bool test_ne(uint x, uint y) { return x != y; }
bool test_ge(uint x, uint y) { return x >= y; }
bool test_gt(uint x, uint y) { return x >  y; }

bool test_lt(int x, int y) { return x <  y; }
bool test_le(int x, int y) { return x <= y; }
bool test_eq(int x, int y) { return x == y; }
bool test_ne(int x, int y) { return x != y; }
bool test_ge(int x, int y) { return x >= y; }
bool test_gt(int x, int y) { return x >  y; }

bool test_lt(ulong x, ulong y) { return x <  y; }
bool test_le(ulong x, ulong y) { return x <= y; }
bool test_eq(ulong x, ulong y) { return x == y; }
bool test_ne(ulong x, ulong y) { return x != y; }
bool test_ge(ulong x, ulong y) { return x >= y; }
bool test_gt(ulong x, ulong y) { return x >  y; }

bool test_lt(long x, long y) { return x <  y; }
bool test_le(long x, long y) { return x <= y; }
bool test_eq(long x, long y) { return x == y; }
bool test_ne(long x, long y) { return x != y; }
bool test_ge(long x, long y) { return x >= y; }
bool test_gt(long x, long y) { return x >  y; }

bool test_lt(float x, float y) { return x <  y; }
bool test_le(float x, float y) { return x <= y; }
bool test_eq(float x, float y) { return x == y; }
bool test_ne(float x, float y) { return x != y; }
bool test_ge(float x, float y) { return x >= y; }
bool test_gt(float x, float y) { return x >  y; }

bool test_lt(double x, double y) { return x <  y; }
bool test_le(double x, double y) { return x <= y; }
bool test_eq(double x, double y) { return x == y; }
bool test_ne(double x, double y) { return x != y; }
bool test_ge(double x, double y) { return x >= y; }
bool test_gt(double x, double y) { return x >  y; }

