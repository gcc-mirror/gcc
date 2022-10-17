// PR c++/87729
// { dg-additional-options -Woverloaded-virtual }

struct S1 { virtual void f(); };
struct S2: S1 {};
struct S3: S1 {};
struct S4: S2, S3 { void f(); };
