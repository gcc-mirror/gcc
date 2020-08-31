// https://issues.dlang.org/show_bug.cgi?id=16087
// { dg-do compile }
import core.simd;

static if (__traits(compiles, void8))   static assert(void8.alignof == 8);
static if (__traits(compiles, double1)) static assert(double1.alignof == 8);
static if (__traits(compiles, float2))  static assert(float2.alignof == 8);
static if (__traits(compiles, byte8))   static assert(byte8.alignof == 8);
static if (__traits(compiles, ubyte8))  static assert(ubyte8.alignof == 8);
static if (__traits(compiles, short4))  static assert(short4.alignof == 8);
static if (__traits(compiles, ushort4)) static assert(ushort4.alignof == 8);
static if (__traits(compiles, int2))    static assert(int2.alignof == 8);
static if (__traits(compiles, uint2))   static assert(uint2.alignof == 8);
static if (__traits(compiles, long1))   static assert(long1.alignof == 8);
static if (__traits(compiles, ulong1))  static assert(ulong1.alignof == 8);

static if (__traits(compiles, void8))   static assert(void8.sizeof == 8);
static if (__traits(compiles, double1)) static assert(double1.sizeof == 8);
static if (__traits(compiles, float2))  static assert(float2.sizeof == 8);
static if (__traits(compiles, byte8))   static assert(byte8.sizeof == 8);
static if (__traits(compiles, ubyte8))  static assert(ubyte8.sizeof == 8);
static if (__traits(compiles, short4))  static assert(short4.sizeof == 8);
static if (__traits(compiles, ushort4)) static assert(ushort4.sizeof == 8);
static if (__traits(compiles, int2))    static assert(int2.sizeof == 8);
static if (__traits(compiles, uint2))   static assert(uint2.sizeof == 8);
static if (__traits(compiles, long1))   static assert(long1.sizeof == 8);
static if (__traits(compiles, ulong1))  static assert(ulong1.sizeof == 8);

static if (__traits(compiles, void16))  static assert(void16.alignof == 16);
static if (__traits(compiles, double2)) static assert(double2.alignof == 16);
static if (__traits(compiles, float4))  static assert(float4.alignof == 16);
static if (__traits(compiles, byte16))  static assert(byte16.alignof == 16);
static if (__traits(compiles, ubyte16)) static assert(ubyte16.alignof == 16);
static if (__traits(compiles, short8))  static assert(short8.alignof == 16);
static if (__traits(compiles, ushort8)) static assert(ushort8.alignof == 16);
static if (__traits(compiles, int4))    static assert(int4.alignof == 16);
static if (__traits(compiles, uint4))   static assert(uint4.alignof == 16);
static if (__traits(compiles, long2))   static assert(long2.alignof == 16);
static if (__traits(compiles, ulong2))  static assert(ulong2.alignof == 16);

static if (__traits(compiles, void16))  static assert(void16.sizeof == 16);
static if (__traits(compiles, double2)) static assert(double2.sizeof == 16);
static if (__traits(compiles, float4))  static assert(float4.sizeof == 16);
static if (__traits(compiles, byte16))  static assert(byte16.sizeof == 16);
static if (__traits(compiles, ubyte16)) static assert(ubyte16.sizeof == 16);
static if (__traits(compiles, short8))  static assert(short8.sizeof == 16);
static if (__traits(compiles, ushort8)) static assert(ushort8.sizeof == 16);
static if (__traits(compiles, int4))    static assert(int4.sizeof == 16);
static if (__traits(compiles, uint4))   static assert(uint4.sizeof == 16);
static if (__traits(compiles, long2))   static assert(long2.sizeof == 16);
static if (__traits(compiles, ulong2))  static assert(ulong2.sizeof == 16);
static if (__traits(compiles, void32))   static assert(void32.alignof == 32);
static if (__traits(compiles, double4))  static assert(double4.alignof == 32);
static if (__traits(compiles, float8))   static assert(float8.alignof == 32);
static if (__traits(compiles, byte32))   static assert(byte32.alignof == 32);
static if (__traits(compiles, ubyte32))  static assert(ubyte32.alignof == 32);
static if (__traits(compiles, short16))  static assert(short16.alignof == 32);
static if (__traits(compiles, ushort16)) static assert(ushort16.alignof == 32);
static if (__traits(compiles, int8))     static assert(int8.alignof == 32);
static if (__traits(compiles, uint8))    static assert(uint8.alignof == 32);
static if (__traits(compiles, long4))    static assert(long4.alignof == 32);
static if (__traits(compiles, ulong4))   static assert(ulong4.alignof == 32);

static if (__traits(compiles, void32))   static assert(void32.sizeof == 32);
static if (__traits(compiles, double4))  static assert(double4.sizeof == 32);
static if (__traits(compiles, float8))   static assert(float8.sizeof == 32);
static if (__traits(compiles, byte32))   static assert(byte32.sizeof == 32);
static if (__traits(compiles, ubyte32))  static assert(ubyte32.sizeof == 32);
static if (__traits(compiles, short16))  static assert(short16.sizeof == 32);
static if (__traits(compiles, ushort16)) static assert(ushort16.sizeof == 32);
static if (__traits(compiles, int8))     static assert(int8.sizeof == 32);
static if (__traits(compiles, uint8))    static assert(uint8.sizeof == 32);
static if (__traits(compiles, long4))    static assert(long4.sizeof == 32);
static if (__traits(compiles, ulong4))   static assert(ulong4.sizeof == 32);

static if (__traits(compiles, void64))   static assert(void64.alignof == 64);
static if (__traits(compiles, double8))  static assert(double8.alignof == 64);
static if (__traits(compiles, float16))  static assert(float16.alignof == 64);
static if (__traits(compiles, byte64))   static assert(byte64.alignof == 64);
static if (__traits(compiles, ubyte64))  static assert(ubyte64.alignof == 64);
static if (__traits(compiles, short32))  static assert(short32.alignof == 64);
static if (__traits(compiles, ushort32)) static assert(ushort32.alignof == 64);
static if (__traits(compiles, int16))    static assert(int16.alignof == 64);
static if (__traits(compiles, uint16))   static assert(uint16.alignof == 64);
static if (__traits(compiles, long8))    static assert(long8.alignof == 64);
static if (__traits(compiles, ulong8))   static assert(ulong8.alignof == 64);

static if (__traits(compiles, void64))   static assert(void64.sizeof == 64);
static if (__traits(compiles, double8))  static assert(double8.sizeof == 64);
static if (__traits(compiles, float16))  static assert(float16.sizeof == 64);
static if (__traits(compiles, byte64))   static assert(byte64.sizeof == 64);
static if (__traits(compiles, ubyte64))  static assert(ubyte64.sizeof == 64);
static if (__traits(compiles, short32))  static assert(short32.sizeof == 64);
static if (__traits(compiles, ushort32)) static assert(ushort32.sizeof == 64);
static if (__traits(compiles, int16))    static assert(int16.sizeof == 64);
static if (__traits(compiles, uint16))   static assert(uint16.sizeof == 64);
static if (__traits(compiles, long8))    static assert(long8.sizeof == 64);
static if (__traits(compiles, ulong8))   static assert(ulong8.sizeof == 64);
