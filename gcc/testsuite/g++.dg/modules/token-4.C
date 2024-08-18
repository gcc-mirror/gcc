// { dg-additional-options -fmodules-ts }
#define MODULE module   // { dg-error "does not name a type" }
export MODULE bob; // { dg-error "may only occur after" }
// { dg-module-cmi !bob }
