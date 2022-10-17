/* { dg-do compile } */
/* { dg-options "-E -Wdate-time" } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdate-time"
const char *date = __DATE__;
_Pragma ("GCC diagnostic pop");
const char *date2 = __DATE__; /* { dg-warning "__DATE__" } */
/* { dg-final { scan-assembler "#pragma GCC diagnostic push" } } */
/* { dg-final { scan-assembler "#pragma GCC diagnostic ignored \"-Wdate-time\"" } } */
/* { dg-final { scan-assembler "#pragma GCC diagnostic pop" } } */
