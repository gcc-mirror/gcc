// Origin: PR debug/45171
// { dg-options "-gdwarf-2 -dA -fno-eliminate-unused-debug-types -fno-debug-types-section" }
// { dg-do compile }

// There should be 2 real instances of byte_size -- one for the
// struct and one for "int".  The other two instances are from
// the comments in the .debug_abbrev section.
// { dg-final { scan-assembler-times "DW_AT_byte_size" 4 } }

typedef struct { int a, b; } x;
