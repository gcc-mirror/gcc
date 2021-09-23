// PR c++/101786
// { dg-do compile { target c++20 } }
// { dg-add-options tls }
// { dg-require-alias "" }
// { dg-require-effective-target tls_runtime }
// { dg-final { scan-assembler-not "_ZTH17mythreadlocalvar1" } }
// { dg-final { scan-assembler "_ZTH17mythreadlocalvar2" } }
// { dg-final { scan-assembler-not "_ZTH17mythreadlocalvar3" } }
// { dg-final { scan-assembler "_ZTH17mythreadlocalvar4" } }

extern thread_local constinit int mythreadlocalvar1;
struct S;
extern thread_local constinit S mythreadlocalvar2;
struct T { int t; };
extern thread_local constinit T mythreadlocalvar3;
struct U { int u; ~U (); };
extern thread_local constinit U mythreadlocalvar4;
int foo () { return mythreadlocalvar1; }
S *bar () { return &mythreadlocalvar2; }
T *baz () { return &mythreadlocalvar3; }
U *qux () { return &mythreadlocalvar4; }
