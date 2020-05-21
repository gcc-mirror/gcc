/* { dg-options "-std=c++11" } */
#include <cstdio>
#include <cstdint>
/* Missing <cinttypes>.  */

int8_t i8;
int16_t i16;
int32_t i32;
int64_t i64;

intptr_t ip;
uintptr_t up;

/* As an identifier.  */
const char *hex8_fmt = PRIx8; /* { dg-error "'PRIx8' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIx8' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *hex16_fmt = PRIx16; /* { dg-error "'PRIx16' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIx16' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *hex32_fmt = PRIx32; /* { dg-error "'PRIx32' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIx32' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *hex64_fmt = PRIx64; /* { dg-error "'PRIx64' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIx64' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *hexptr_fmt = PRIxPTR; /* { dg-error "'PRIxPTR' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIxPTR' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */

void test_printf (void)
{
  printf ("some format strings %s, %s, %s, %s, %s, %s\n",
	  PRId8, /* { dg-error "'PRId8' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRId8' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
	  PRIi16, /* { dg-error "'PRIi16' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIi16' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
	  PRIo32, /* { dg-error "'PRIo32' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIo32' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
	  PRIu64, /* { dg-error "'PRIu64' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIu64' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
	  PRIx32, /* { dg-error "'PRIx32' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIx32' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
	  PRIoPTR);  /* { dg-error "'PRIoPTR' was not declared" "undeclared identifier" { target *-*-* } } */
/* { dg-message "'PRIoPTR' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
}
