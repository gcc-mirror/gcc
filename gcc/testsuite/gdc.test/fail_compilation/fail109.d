/*
TEST_OUTPUT:
---
fail_compilation/fail109.d(12): Error: enum member `fail109.Bool.Unknown` initialization with `Bool.True+1` causes overflow for type `bool`
---
*/

enum Bool : bool
{
    False,
    True,
    Unknown
}

/* https://issues.dlang.org/show_bug.cgi?id=11088
TEST_OUTPUT:
---
fail_compilation/fail109.d(25): Error: enum member `fail109.E.B` initialization with `E.A+1` causes overflow for type `int`
fail_compilation/fail109.d(31): Error: enum member `fail109.E1.B` initialization with `E1.A+1` causes overflow for type `short`
---
*/
enum E
{
    A = int.max,
    B
}

enum E1 : short
{
    A = short.max,
    B
}

/* https://issues.dlang.org/show_bug.cgi?id=14950
TEST_OUTPUT:
---
fail_compilation/fail109.d(50): Error: cannot check `fail109.B.end` value for overflow
fail_compilation/fail109.d(50): Error: comparison between different enumeration types `B` and `C`; If this behavior is intended consider using `std.conv.asOriginalType`
fail_compilation/fail109.d(50): Error: enum member `fail109.B.end` initialization with `B.start+1` causes overflow for type `C`
---
*/
enum C
{
    start,
    end
}
enum B
{
    start = C.end,
    end
}

/* https://issues.dlang.org/show_bug.cgi?id=11849
TEST_OUTPUT:
---
fail_compilation/fail109.d(72): Error: enum member `fail109.RegValueType1a.Unknown` is forward referenced looking for `.max`
fail_compilation/fail109.d(79): Error: enum member `fail109.RegValueType1b.Unknown` is forward referenced looking for `.max`
fail_compilation/fail109.d(84): Error: enum member `fail109.RegValueType2a.Unknown` is forward referenced looking for `.min`
fail_compilation/fail109.d(91): Error: enum member `fail109.RegValueType2b.Unknown` is forward referenced looking for `.min`
---
*/

alias DWORD = uint;

enum : DWORD
{
    REG_DWORD = 4
}

enum RegValueType1a : DWORD
{
    Unknown = DWORD.max,
    DWORD = REG_DWORD,
}

enum RegValueType1b : DWORD
{
    DWORD = REG_DWORD,
    Unknown = DWORD.max,
}

enum RegValueType2a : DWORD
{
    Unknown = DWORD.min,
    DWORD = REG_DWORD,
}

enum RegValueType2b : DWORD
{
    DWORD = REG_DWORD,
    Unknown = DWORD.min,
}

/*
TEST_OUTPUT:
---
fail_compilation/fail109.d(107): Error: enum member `fail109.d` initialization with `__anonymous.c+1` causes overflow for type `Q`
---
*/

struct Q {
	enum max = Q();
}

enum {
	c = Q(),
	d
}
