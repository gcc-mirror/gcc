// { dg-module-do run }

// { dg-additional-options -fmodules-ts }
export module frob;
// { dg-module-bmi frob }

export typedef struct { int m; } frob;
