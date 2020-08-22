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

/* As a part of a string-literal.  */
const char *dec8msg_fmt = "Provide %" PRId8 "\n"; /* { dg-error "expected" "expected string-literal" { target *-*-* } } */
/* { dg-message "'PRId8' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *dec16msg_fmt = "Provide %" PRId16 "\n"; /* { dg-error "expected" "expected string-literal" { target *-*-* } } */
/* { dg-message "'PRId16' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *dec32msg_fmt = "Provide %" PRId32 "\n"; /* { dg-error "expected" "expected string-literal" { target *-*-* } } */
/* { dg-message "'PRId32' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *dec64msg_fmt = "Provide %" PRId64 "\n"; /* { dg-error "expected" "expected string-literal" { target *-*-* } } */
/* { dg-message "'PRId64' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
const char *decptrmsg_fmt = "Provide %" PRIdPTR "\n"; /* { dg-error "expected" "expected string-literal" { target *-*-* } } */
/* { dg-message "'PRIdPTR' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */

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

  printf ("%" PRIo8 "\n", i8); /* { dg-error "expected" } */
/* { dg-message "'PRIo8' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  printf ("%" PRIo16 "\n", i16); /* { dg-error "expected" } */
/* { dg-message "'PRIo16' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  printf ("%" PRIo32 "\n", i32); /* { dg-error "expected" } */
/* { dg-message "'PRIo32' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  printf ("%" PRIo64 "\n", i64); /* { dg-error "expected" } */
/* { dg-message "'PRIo64' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  printf ("%" PRIoPTR "\n", ip); /* { dg-error "expected" } */
/* { dg-message "'PRIoPTR' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
}

void test_scanf (void)
{
  scanf ("%" SCNu8 "\n", &i8); /* { dg-error "expected" } */
/* { dg-message "'SCNu8' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  scanf ("%" SCNu16 "\n", &i16); /* { dg-error "expected" } */
/* { dg-message "'SCNu16' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  scanf ("%" SCNu32 "\n", &i32); /* { dg-error "expected" } */
/* { dg-message "'SCNu32' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  scanf ("%" SCNu64 "\n", &i64); /* { dg-error "expected" } */
/* { dg-message "'SCNu64' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  scanf ("%" SCNuPTR "\n", &ip); /* { dg-error "expected" } */
/* { dg-message "'SCNuPTR' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
  scanf ("%" SCNxPTR "\n", &up); /* { dg-error "expected" } */
/* { dg-message "'SCNxPTR' is defined in header '<cinttypes>'; did you forget to '#include <cinttypes>'?" "replacement note" { target *-*-* } .-1 } */
}
