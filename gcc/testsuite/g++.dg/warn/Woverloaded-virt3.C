// PR c++/87729
// { dg-additional-options -Woverloaded-virtual }

struct S1 {};
struct S2: S1 { virtual ~S2(); };
struct S3 { virtual ~S3(); };
struct S4: S2, S3 { virtual ~S4(); };
