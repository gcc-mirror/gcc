// PR c++/69585
// { dg-do compile { target c++11 } }

struct __attribute__ ((aligned (2))) __attribute__ ((may_alias))
S1 { };

struct __attribute__ ((aligned (2))) [[gnu::may_alias]]
S2 { };

struct alignas (2) __attribute__ ((may_alias))
S3 { };

struct alignas (2) [[gnu::may_alias]]
S4 { };


struct __attribute__ ((may_alias)) __attribute__ ((aligned (2)))
S1_2 { };

struct [[gnu::may_alias]] __attribute__ ((aligned (2)))
S2_2 { };

struct __attribute__ ((may_alias)) alignas (2)
S3_2 { };

struct [[gnu::may_alias]] alignas (2)
S4_2 { };
