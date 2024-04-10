// REQUIRED_ARGS: -mcpu=avx2

import core.simd;


version = CondExp;
//version = ReturnInfer;

T rvalueOf(T)();

version (CondExp)
{
    alias X(T, U) = typeof(0 ? rvalueOf!T : rvalueOf!U);
}
else version (ReturnInfer)
{
    alias X(T, U) = typeof({ if (0) return rvalueOf!T; else return rvalueOf!U;}());
}
else
    static assert(0);

enum Error(T, U) = !__traits(compiles, X!(T, U));


interface I {}
class B {}
class C : B, I {}
class D : B {}
class K {}
struct SI { I o; alias o this; }
struct SC { C o; alias o this; }
struct SB { B o; alias o this; }
struct SD { D o; alias o this; }
struct SK { K o; alias o this; }
struct SiC { immutable(C) o; alias o this; }

struct S { int a; }
struct Si { int a; alias a this; }
struct Si2 { int a; alias a this; }
struct Sl { long a; alias a this; }

/******************************
 * Basic types
 */

static assert(is( X!( byte, byte ) == byte ));
static assert(is( X!( ubyte, ubyte ) == ubyte ));
static assert(is( X!( byte, ubyte ) == int ));
static assert(is( X!( byte, short ) == int ));
static assert(is( X!( byte, ushort ) == int ));
static assert(is( X!( byte, const(byte) ) == int ));
static assert(is( X!( ubyte, const(ubyte) ) == int ));

static assert(is( X!( short, short ) == short ));
static assert(is( X!( ushort, ushort ) == ushort ));
static assert(is( X!( short, ushort ) == int ));
static assert(is( X!( short, const(short) ) == int ));
static assert(is( X!( ushort, const(ushort) ) == int ));

static assert(is( X!( int, int ) == int ));
static assert(is( X!( int, uint ) == uint ));
static assert(is( X!( uint, uint ) == uint ));

static assert(is( X!( int, long ) == long ));
static assert(is( X!( int, ulong ) == ulong ));

static assert(is( X!( float, float ) == float ));
static assert(is( X!( float, int ) == float ));
static assert(is( X!( float, uint ) == float ));
static assert(is( X!( float, long ) == float ));
static assert(is( X!( float, ulong ) == float ));
static assert(is( X!( float, double ) == double ));
static assert(is( X!( float, real ) == real ));

static assert(is( X!( char, char ) == char ));
static assert(is( X!( char, byte ) == int ));
static assert(is( X!( char, ubyte ) == int ));
static assert(is( X!( char, wchar ) == dchar ));
static assert(is( X!( char, dchar ) == dchar ));
static assert(is( X!( char, const(char) ) == const(char) ));
static assert(is( X!( wchar, const(wchar) ) == const(wchar) ));
static assert(is( X!( dchar, const(dchar) ) == const(dchar) ));
static assert(is( X!( char, immutable(char) ) == const(char) ));
static assert(Error!( char, shared(char) ));

static assert(is( X!( char, float ) == float ));

static assert(is( X!( immutable(int), int ) == int ));
static assert(is( X!( const(int), int ) == int ));
static assert(is( X!( shared(int), int ) == int ));
static assert(is( X!( immutable(int), const(shared(int)) ) == int ));
static assert(is( X!( shared(int), const(int) ) == int ));

/******************************
 * Strings
 */

static assert(is( X!( string, string ) == string ));
static assert(Error!( wstring, string ));
static assert(Error!( dstring, string ));
static assert(Error!( dstring, wstring ));
static assert(is( X!( const(char)[], string ) == const(char)[] ));
static assert(is( X!( char[], string ) == const(char)[] ));
static assert(is( X!( string, immutable(string) ) == immutable(string) )); // `const`
static assert(is( X!( immutable(string), string ) == string )); // not commutative

/******************************
 * Enums
 */

enum Ei : int { a, }
enum Eb : byte { a, }
enum Ec : char { a, }
enum Ew : wchar { a, }

static assert(is( X!( Ei, Ei ) == Ei ));
static assert(is( X!( Ei, const(Ei) ) == const(Ei) ));
static assert(is( X!( Ei, immutable(Ei) ) == const(Ei) ));
static assert(is( X!( Eb, Eb ) == Eb ));
static assert(is( X!( Eb, const(Eb) ) == const(Eb) ));
static assert(is( X!( Eb, immutable(Eb) ) == const(Eb) ));
static assert(is( X!( Ei, Eb ) == int ));
static assert(is( X!( Ei, const(Eb) ) == int ));
static assert(is( X!( Ei, immutable(Eb) ) == int ));

static assert(is( X!( Ec, Ec ) == Ec ));
static assert(is( X!( Ec, const(Ec) ) == const(Ec) ));
static assert(is( X!( Ec, immutable(Ec) ) == const(Ec) ));
static assert(is( X!( Ew, Ew ) == Ew ));
static assert(is( X!( Ew, const(Ew) ) == const(Ew) ));
static assert(is( X!( Ew, immutable(Ew) ) == const(Ew) ));
static assert(is( X!( Ew, Ec ) == dchar ));
static assert(is( X!( Ew, const(Ec) ) == dchar ));
static assert(is( X!( Ew, immutable(Ec) ) == dchar ));

/******************************
 * Tuple
 */

alias Tuple(Args...) = Args;
static assert(!__traits(compiles ,typeof(0 ? Tuple!1 : Tuple!1)));

/******************************
 * Pointers
 */

static assert(is( X!( int*, int* ) == int* ));
static assert(is( X!( const(int*), const(int)* ) == const(int*) ));
static assert(is( X!( const(int)*, const(int*) ) == const(int)* )); // not commutative
static assert(Error!( uint*, int* ));
static assert(is( X!( int function(), int function() ) == int function() ));

// void pointer
static assert(is( X!( void*, int* ) == void* ));
static assert(is( X!( int*, void* ) == void* ));
static assert(is( X!( const(int)*, void* ) == void* ));
static assert(is( X!( const(int*), void* ) == void* ));
static assert(is( X!( int*, const(void)* ) == const(void)* )); // `const`
static assert(is( X!( int*, const(void*) ) == const(void*) )); // `const`
static assert(is( X!( int*, shared(void*) ) == shared(void*) )); // should fail
static assert(is( X!( int*, shared(void)* ) == shared(void)* )); // should fail

static assert(Error!( int**, void** )); // should work

static assert(is( X!( void*, int function() ) == void* ));
static assert(is( X!( immutable(void*), int function() ) == immutable(void*) )); // `const`

// implicit conversion
static assert(is( X!( int*, const(int*) ) == const(int*) ));
static assert(is( X!( int*, const(int)* ) == const(int)* ));
static assert(is( X!( int***, const(int)*** ) == const(int**)* ));
static assert(is( X!( immutable(int)***, const(int)*** ) == const(int**)* ));
static assert(is( X!( immutable(int)***, const(shared(int))*** ) == const(shared(int)**)* ));

// common modifier
static assert(is( X!( immutable(int)*, int* ) == const(int)* ));
static assert(is( X!( immutable(int*), int* ) == const(int)* ));
static assert(is( X!( immutable(int)*, shared(int)* ) == shared(const(int))* ));
static assert(Error!( shared(int)*, int* ));

static assert(is( X!( immutable(int)***, int*** ) == const(int**)* )); // `const(int)***`
static assert(is( X!( shared(const(int)***), shared(int***) ) == const(shared(int*)*)* )); // `shared(const(int)***)`
static assert(is( X!( shared(const(int)***), shared(int**)* ) == const(shared(int*)*)* ));
static assert(Error!( shared(const(int)***), shared(int*)** ));


// class pointer
static assert(is( X!( C*, B* ) == B* ));
static assert(is( X!( const(C)*, B* ) == const(B)* ));
static assert(Error!( shared(C)*, B* ));
static assert(is( X!( immutable(C)*, B* ) == const(B)* ));
static assert(is( X!( immutable(C*), B* ) == const(B)* ));
static assert(is( X!( C**, B** ) == const(B*)* )); // `B**`
static assert(is( X!( B**, C** ) == const(B*)* )); // `B**`
static assert(is( X!( C***, B*** ) == const(B**)* )); // `B***`

static assert(is( X!( C*, I* ) == I* ));
static assert(is( X!( I*, C* ) == I* ));
static assert(Error!( C**, I** ));

static assert(Error!( C*, D* )); // should work

// function pointer
static assert(is( X!( immutable(int function()), int function() ) == immutable(int function()) ));
static assert(is( X!( int function(), immutable(int function()) ) == int function() )); // not commutative
static assert(is( X!( int function(), shared(int function()) ) == int function() ));

static assert(is( X!( int function(), const(int) function() ) == const(int) function() ));
static assert(is( X!( int function(), immutable(int) function() ) == immutable(int) function() )); // `const`
static assert(is( X!( immutable(int) function(), int function() ) == int function() )); // not commutative

static assert(Error!( uint function(), int function() ));

static assert(is( X!( C function(), B function() ) == B function() ));
static assert(is( X!( B function(), C function() ) == B function() ));
static assert(is( X!( C function(), const(B) function() ) == const(B) function() ));
static assert(Error!( const(C) function(), B function() )); // should work
static assert(Error!( C* function(), B* function() )); // should work
static assert(Error!( C function(), I function() ));
static assert(Error!( C* function(), I* function() ));

static assert(is( X!( C delegate(), B delegate() ) == B delegate() ));
static assert(Error!( C delegate(), I delegate() ));

static assert(Error!( C function(), D function() )); // should work

static assert(Error!( void function(int), void function(const(int)) )); // should work

static assert(Error!( void function(C), void function(B) ));
static assert(Error!( void function(C*), void function(B*) ));
static assert(Error!( void function(const(C)*), void function(B*) ));
static assert(is( X!( void function(C*), void function(const(B*)) ) == void function(C*) )); // !?

static assert(is( X!( void function(C), void function(const(C)) ) == void function(C) ));
static assert(is( X!( void function(C), void function(const(C)) ) == void function(C) ));

static assert(is( X!( void function() pure nothrow @nogc @safe, void function() ) == void function() ));
static assert(is( X!( void function() pure @safe, void function() @nogc ) == void function() ));
static assert(is( X!( void function() pure @trusted, void function() nothrow @safe ) == void function() @trusted ));
static assert(is( X!( void function() @trusted, void function() ) == void function()));
static assert(is( X!( void function() @trusted, void function() @trusted ) == void function() @trusted ));
static assert(is( X!( void function() @safe, void function() @trusted ) == void function() @trusted ));
static assert(is( X!( void function() @trusted, void function() @safe ) == void function() @safe )); // not commutative

static assert(is( X!( const(int function())*, int function()* ) == const(int function())* ));
static assert(is( X!( immutable(int function())*, int function()* ) == const(int function())* ));
static assert(Error!( shared(int function())*, int function()* ));
static assert(is( X!( shared(int function())*, immutable(int function())* ) == shared(const(int function()))* ));


/******************************
 * Arrays
 */

static assert(is( X!( int[4], int[4] ) == int[4] ));
static assert(is( X!( int[], int[] ) == int[] ));

// static array modifier conversion
static assert(is( X!( const(int)[4], int[4] ) == int[4] ));
static assert(is( X!( int[4], const(int)[4] ) == const(int)[4] )); // not commutative
static assert(is( X!( const(int)[4], immutable(int)[4] ) == immutable(int)[4] ));
static assert(is( X!( immutable(int)[4], const(int)[4] ) == const(int)[4] )); // not commutative
static assert(is( X!( int[4], immutable(int)[4] ) == immutable(int)[4] ));
static assert(is( X!( immutable(int)[4], int[4] ) == int[4] )); // not commutative
static assert(Error!( shared(int)[4], int[4] )); // should work
static assert(Error!( int[4], shared(int)[4] )); // should work
static assert(Error!( shared(int)[4], const(int)[4] )); // should work
static assert(Error!( const(int)[4] , shared(int)[4])); // should work
static assert(is( X!( shared(const(int))[4], shared(int)[4] ) == shared(int)[4] ));
static assert(is( X!( shared(int)[4], shared(const(int))[4] ) == shared(const(int))[4] )); // not commutative
static assert(is( X!( shared(const(int))[4], immutable(int)[4] ) == shared(const(int))[4] ));
static assert(is( X!( immutable(int)[4], shared(const(int))[4] ) == shared(const(int))[4] ));

static assert(is( X!( immutable(int)[4], shared(int)[4] ) == shared(const(int))[] )); // `[4]`
static assert(is( X!( shared(int)[4], immutable(int)[4] ) == shared(const(int))[] )); // `[4]`

static assert(is( X!( int*[4], const(int)*[4] ) == const(int)*[4]));
static assert(is( X!( const(int)*[4], int*[4] ) == const(int)*[4]));
static assert(is( X!( immutable(int)*[4], const(int)*[4] ) == const(int)*[4]));
static assert(is( X!( const(int)*[4], immutable(int)*[4] ) == const(int)*[4]));
static assert(Error!( int*[4], immutable(int)*[4] )); // should work
static assert(Error!( immutable(int)*[4], int*[4] )); // should work
static assert(Error!( int*[4], shared(int)*[4] ));
static assert(Error!( shared(int)*[4], int*[4] ));
static assert(Error!( shared(int)*[4], immutable(int)*[4] )); // should work
static assert(Error!( immutable(int)*[4], shared(int)*[4] )); // should work
static assert(is( X!( shared(const(int))*[4], shared(int)*[4] ) == shared(const(int))*[4] ));
static assert(is( X!( shared(int)*[4], shared(const(int))*[4] ) == shared(const(int))*[4] ));

static assert(is( X!( C[4], const(C)[4] ) == const(C)[4] ));
static assert(is( X!( const(C)[4], C[4] ) == const(C)[4] ));
static assert(is( X!( C[4], immutable(C)[4] ) == const(C)[] )); // `[4]`
static assert(is( X!( immutable(C)[4], C[4] ) == const(C)[] )); // `[4]`
static assert(Error!( shared(C)[4], C[4] ));
static assert(Error!( C[4], shared(C)[4] ));
static assert(is( X!( shared(C)[4], immutable(C)[4] ) == shared(const(C))[] )); // `[4]`
static assert(is( X!( immutable(C)[4], shared(C)[4] ) == shared(const(C))[] )); // `[4]`
static assert(is( X!( shared(const(C))[4], shared(C)[4] ) == shared(const(C))[4] ));
static assert(is( X!( shared(C)[4], shared(const(C))[4] ) == shared(const(C))[4] ));

// base class conversion
static assert(is( X!(C[4], B[4]) ));
static assert(Error!( C[4], I[4] ));
static assert(Error!( C[4], D[4] ));
static assert(is( X!( C[4], const(B)[4] ) == const(B)[4] ));
static assert(Error!( C[4], const(I)[4] ));
static assert(Error!( C[4], const(D)[4] ));
static assert(Error!( C*[4], B*[4] ));
static assert(Error!( C*[4], I*[4] ));
static assert(Error!( C*[4], D*[4] ));
static assert(is( X!( C*[4], const(B*)[4] ) == const(B*)[] )); // !?
static assert(Error!( C*[4], const(I*)[4] ));
static assert(Error!( C*[4], const(D*)[4] ));
static assert(Error!( C*[4], B**[4] ));
static assert(Error!( C*[4], const(B*)*[4] ));
static assert(Error!( C*[4], const(B**)[4] ));

// static to dynamic
static assert(is( X!( int[4], void[4] ) == void[] ));
static assert(is( X!( void[4], int[4] ) == void[] ));
static assert(is( X!( int[4], int[3] ) == int[] ));
static assert(is( X!( int[4], const(int)[3] ) == const(int)[] ));
static assert(is( X!( int[4], int[] ) == int[] ));
static assert(is( X!( const(int)[4], int[] ) == const(int)[] ));
static assert(is( X!( int[4], const(int)[] ) == const(int)[] ));
static assert(is( X!( int[4], void[] ) == void[] ));
static assert(is( X!( const(int)[4], void[] ) == const(void)[] ));
static assert(Error!( int*[4], void*[4] )); // should work
static assert(Error!( int*[4], void*[] )); // should work
static assert(is( X!( int*[4], int*[] ) == int*[] ));
static assert(is( X!( const(int*)[4], int*[] ) == const(int*)[] ));
static assert(is( X!( int*[4], const(int*)[] ) == const(int*)[] ));
static assert(Error!( const(int)*[4], int*[] )); // should work
static assert(Error!( int*[4], const(int)*[] )); // should work
static assert(Error!( int[4], long[] ));
static assert(Error!( int[4], uint[] ));
static assert(Error!( int[4], short[] ));
static assert(Error!( C[4], B[] ));
static assert(Error!( C[4], I[] ));
static assert(Error!( C[4], D[] ));
static assert(Error!( C*[4], B*[] ));
static assert(Error!( C*[4], I*[] ));
static assert(Error!( C*[4], D*[] ));

// dynamic arrays
static assert(is( X!( int[], int[] ) == int[] ));
static assert(is( X!( int[], const(int)[] ) == const(int)[] ));
static assert(is( X!( const(int)[], int[] ) == const(int)[] ));
static assert(is( X!( int[], immutable(int)[] ) == const(int)[] ));
static assert(is( X!( immutable(int)[], int[] ) == const(int)[] ));
static assert(Error!( int[], shared(int)[] ));
static assert(Error!( shared(int)[], int[] ));
static assert(is( X!( shared(int)[], immutable(int)[] ) == shared(const(int))[] ));
static assert(is( X!( immutable(int)[], shared(int)[] ) == shared(const(int))[] ));
static assert(Error!( const(int)[], shared(int)[] ));
static assert(Error!( shared(int)[], const(int)[] ));

static assert(is( X!( int[], void[] ) == void[] ));
static assert(is( X!( void[], int[] ) == void[] ));
static assert(is( X!( int[], const(void)[] ) == const(void)[] ));
static assert(is( X!( const(int)[], void[] ) == const(void)[] ));

static assert(is( X!( int*[], const(int*)[] ) == const(int*)[] ));
static assert(is( X!( const(int*)[], int*[] ) == const(int*)[] ));
static assert(Error!( int*[], const(int)*[] )); // should work
static assert(Error!( const(int)*[], int*[] )); // should work

static assert(is( X!( C[], const(C)[] ) == const(C)[] ));
static assert(is( X!( const(C)[], C[] ) == const(C)[] ));

static assert(Error!( int[], long[] ));
static assert(Error!( int[], uint[] ));
static assert(Error!( int[], short[] ));

static assert(Error!( C[], B[] ));
static assert(Error!( C[], I[] ));
static assert(Error!( C[], D[] ));
static assert(Error!( C*[], B*[] ));
static assert(Error!( C*[], I*[] ));
static assert(Error!( C*[], D*[] ));

/******************************
 * Associative arrays
 */

static assert(is( X!( int[int], int[int] ) == int[int] ));
static assert(Error!( const(int[int]), int[int] )); // should work
static assert(Error!( immutable(int[int]), int[int] )); // should work
static assert(Error!( shared(int[int]), int[int] ));

/******************************
 * Classes
 */

static assert(Error!( C, void* ));
static assert(Error!( void*, C ));

static assert(is( X!( C, C ) == C ));
static assert(is( X!( C, B ) == B ));
static assert(is( X!( C, I ) == I ));
static assert(is( X!( C, D ) == B ));
static assert(is( X!( C, K ) == Object ));

static assert(is( X!( C, SC ) == C ));
static assert(is( X!( C, SB ) == B ));
static assert(is( X!( C, SI ) == I ));
static assert(is( X!( C, SD ) == B ));
static assert(is( X!( C, SK ) == Object ));

static assert(is( X!( C, immutable(C) ) == const(C) ));
static assert(is( X!( C, immutable(I) ) == const(I) ));
static assert(is( X!( C, immutable(B) ) == const(B) ));
static assert(is( X!( C, immutable(D) ) == const(B) ));
static assert(is( X!( C, immutable(K) ) == const(Object) ));

static assert(is( X!( C, immutable(SC)) == const(B) ));
static assert(is( X!( C, immutable(SI) ) == const(I) ));
static assert(is( X!( immutable(SI), C ) == const(I) ));
static assert(is( X!( C, immutable(SB) ) == const(Object) ));
static assert(is( X!( C, immutable(SD) ) == const(B) ));
static assert(is( X!( C, immutable(SK) ) == const(Object) ));

static assert(is( X!( const(C), C ) == const(C) ));
static assert(is( X!( const(C), I ) == const(I) ));
static assert(is( X!( const(C), B ) == const(B) ));
static assert(is( X!( const(C), D ) == const(B) ));
static assert(is( X!( const(C), K ) == const(Object) ));

static assert(is( X!( const(C), SC ) == const(C)));
static assert(is( X!( const(C), SI ) == const(I)));
static assert(is( X!( const(SI), const(C) ) == const(I) ));
static assert(is( X!( const(C), SB ) == const(Object)));
static assert(is( X!( const(C), SD ) == const(B)));
static assert(is( X!( const(C), SK ) == const(Object)));

static assert(is( X!( SiC, SC ) == const(C) ));

/******************************
 * Structs
 */

static assert(is( X!( S, S ) == S ));
static assert(is( X!( S, immutable(S) ) == const(S) ));
static assert(Error!( S, shared(S) ));
static assert(is( X!( Si, Si ) == Si ));
static assert(is( X!( Si, int ) == int ));
static assert(is( X!( int, Si ) == int ));
static assert(is( X!( Si, Si2 ) == int ));

static assert(is( X!( int, Sl ) == long ));
static assert(is( X!( Si, Sl ) == long ));


/******************************
 * Vectors
 */

static if (__traits(compiles, int4))
{
    static assert(is( X!( int4, int4 ) == int4));
    static assert(is( X!( int4, const(int4) ) == const(int4)));
    static assert(is( X!( int4, immutable(int4) ) == const(int4)));
    static assert(is( X!( __vector(const(int)[4]), int4 ) == int4));
    static assert(is( X!( int4, __vector(const(int)[4]) ) == int4));

    static assert(Error!( int[4], int4 ));
    static assert(Error!( int4, int[4] ));
}

static if (__traits(compiles, { byte16 a; void16 b; }))  static assert(Error!( byte16, void16 ));
static if (__traits(compiles, { int4 a; void16 b; }))    static assert(Error!( int4, void16 ));
static if (__traits(compiles, { float4 a; void16 b; }))  static assert(Error!( float4, void16 ));
static if (__traits(compiles, { byte16 a; ubyte16 b; })) static assert(Error!( byte16, ubyte16 ));
static if (__traits(compiles, { short8 a; ushort8 b; })) static assert(Error!( short8, ushort8 ));
static if (__traits(compiles, { int4 a; uint4 b; }))     static assert(Error!( int4, uint4 ));
static if (__traits(compiles, { int4 a; float4 b; }))    static assert(Error!( int4, float4 ));
static if (__traits(compiles, { long4 a; ulong4 b; }))   static assert(Error!( long4, ulong4 ));
static if (__traits(compiles, { double4 a; float8 b; })) static assert(Error!( double4, float8 ));

/******************************
 * Null
 */

static assert(is( X!( typeof(null), int* ) == int*));
static assert(is( X!( typeof(null), int[] ) == int[]));
static assert(is( X!( typeof(null), int[int] ) == int[int]));
