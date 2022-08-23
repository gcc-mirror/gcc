// DFLAGS:
// PERMUTE_ARGS:
// POST_SCRIPT: compilable/extra-files/minimal/verify_symbols.sh
// REQUIRED_ARGS: -defaultlib=
// EXTRA_SOURCES: extra-files/minimal/object.d

// This test ensures an empty main with a struct and enum, built with a minimal
// runtime, does not generate ModuleInfo or exception handling code, and does not
// require TypeInfo

struct S { }

enum E
{
    e0 = 0,
    e1 = 1
}

void main() { }
