/*
REQUIRED_ARGS: -de -m64
TEST_OUTPUT:
---
fail_compilation/foreach_index_overflow.d(19): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
fail_compilation/foreach_index_overflow.d(21): Deprecation: foreach: loop index implicitly converted from `size_t` to `ushort`
fail_compilation/foreach_index_overflow.d(24): Deprecation: foreach: loop index implicitly converted from `size_t` to `ubyte`
fail_compilation/foreach_index_overflow.d(26): Deprecation: foreach: loop index implicitly converted from `size_t` to `byte`
---
*/

void main()
{
    enum { red, green, blue }
    foreach (int i, color; [red, green, blue]) {} // OK

    int[] arr;
    foreach (int index, element; arr[0 .. 0x8000_0000]) {} // OK
    foreach (int index, element; arr[0 .. 0x8000_0001]) {} // error
    foreach (ushort index, element; arr[0 .. 0x1_0000]) {} // OK
    foreach (ushort index, element; arr[0 .. 0x1_0001]) {} // error

    int[257] data;
    foreach (ubyte i, x; data[]) {} // error
    foreach (ubyte i, x; data[0..256]) {} // OK
    foreach (byte i, x; data[0..0x81]) {} // error
    foreach (byte i, x; data[0..0x80]) {} // OK
}
