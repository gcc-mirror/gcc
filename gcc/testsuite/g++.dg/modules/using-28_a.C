// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }
// Test that typedef names correctly provide external linkage

module;
typedef struct { int x; } A;
export module M;

export typedef struct {} B;

export using ::A;
export using ::B;
