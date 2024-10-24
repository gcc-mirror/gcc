// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

struct alignas(16) Align {};
struct Final final {};
struct NeedsConstructing { NeedsConstructing(); };
