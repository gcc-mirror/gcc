// PR c++/67533
// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }

struct Tls {};
void _ZTW5mytls();
thread_local Tls mytls = mytls; // { dg-error "" }
