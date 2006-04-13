/* Character arrays but not arrays of compatible enum type may be
   initialized by narrow string literals.  Arrays of type compatible
   with wchar_t, including compatible enums, may be initialized by
   wide string literals.  Use -fshort-enums -fshort-wchar so the
   relevant circumstances can be obtained portably; may still fail if
   char, short and int do not all have distinct precisions.  */
/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-std=c99 -pedantic-errors -fshort-enums -fshort-wchar" } */

#include <limits.h>
#include <stddef.h>

typedef enum { schar_min = SCHAR_MIN, schar_max = SCHAR_MAX } schar;
typedef enum { uchar_max = UCHAR_MAX } uchar;
typedef enum { shrt_min = SHRT_MIN, shrt_max = SHRT_MAX } sshrt;
typedef enum { ushrt_max = USHRT_MAX } ushrt;

char a0[] = "foo";
const signed char a2[4] = "foo";
volatile unsigned char a3[3] = "foo";
wchar_t a4[] = L"foo";
const wchar_t a5[3] = L"foo";
volatile ushrt a6[] = L"foo";

schar a7[] = "foo"; /* { dg-error "string constant" "a7" } */
uchar a8[] = "foo"; /* { dg-error "string constant" "a8" } */
const schar a9[] = "foo"; /* { dg-error "string constant" "a9" } */
short a10[] = L"foo"; /* { dg-error "string constant" "a10" } */
const sshrt a11[] = L"foo"; /* { dg-error "string constant" "a11" } */
char a12[] = L"foo"; /* { dg-error "from wide string" "a12" } */
wchar_t a13[] = "foo"; /* { dg-error "non-wide string" "a13" } */

char b0[] = { "foo" };
const signed char b2[4] = { "foo" };
volatile unsigned char b3[3] = { "foo" };
wchar_t b4[] = { L"foo" };
const wchar_t b5[3] = { L"foo" };
volatile ushrt b6[] = { L"foo" };

schar b7[] = { "foo" }; /* { dg-error "string constant" "b7" } */
uchar b8[] = { "foo" }; /* { dg-error "string constant" "b8" } */
const schar b9[] = { "foo" }; /* { dg-error "string constant" "b9" } */
short b10[] = { L"foo" }; /* { dg-error "string constant" "b10" } */
const sshrt b11[] = { L"foo" }; /* { dg-error "string constant" "b11" } */
char b12[] = { L"foo" }; /* { dg-error "from wide string" "b12" } */
wchar_t b13[] = { "foo" }; /* { dg-error "non-wide string" "b13" } */

struct s { signed char a[10]; int b; ushrt c[10]; };

struct s c = { "foo", 0, L"bar" };
struct s d = { .c = L"bar", .a = "foo" };

ushrt *e = (ushrt [7]){ L"bar" };

wchar_t f[5][5] = { L"foo", L"bar" };
ushrt g[5][5] = { L"foo", L"bar" };
