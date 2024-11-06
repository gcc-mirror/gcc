/* { dg-do preprocess } */
/* { dg-options "-traditional-cpp" } */

#if __has_embed(__FILE__ limit(6))
#endif
/* { dg-error "-:'__has_embed' not supported in traditional C" "" { target *-*-* } .-2 } */
/* { dg-error "-:missing binary operator before token '\\\('" "" { target *-*-* } .-3 } */
#define FOO 20000,20001,20002
#define BAR 30000,30001,30002
#embed __FILE__ limit (4) prefix(10000,10001,10002+) suffix(+10003,10004,10005)
/* { dg-error "-:'#embed' not supported in traditional C" "" { target *-*-* } .-1 } */
#embed __FILE__ limit (6) prefix(FOO,) suffix(,BAR)
/* { dg-error "-:'#embed' not supported in traditional C" "" { target *-*-* } .-1 } */
