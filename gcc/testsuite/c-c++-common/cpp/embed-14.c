/* { dg-do preprocess } */
/* { dg-options "-P -fdirectives-only" } */

#if __has_embed(__FILE__ limit(6))
#define FOO 20000,20001,20002
#define BAR 30000,30001,30002
#embed __FILE__ limit (4) prefix(10000,10001,10002+) suffix(+10003,10004,10005)
#embed __FILE__ limit (6) prefix(FOO,) suffix(,BAR)
#endif

/* { dg-final { scan-file embed-14.i "10000,10001,10002\\\+\[ \t\n\r]*\[0-9]+,\[0-9]+,\[0-9]+,\[0-9]+\[ \t\n\r]*\\\+10003,10004,10005" } } */
/* { dg-final { scan-file embed-14.i "20000,20001,20002,\[ \t\n\r]*\[0-9]+,\[0-9]+,\[0-9]+,\[0-9]+,\[0-9]+,\[0-9]+\[ \t\n\r]*,30000,30001,30002" } } */
/* { dg-final { scan-file-not embed-14.i "\[#]embed" } } */
