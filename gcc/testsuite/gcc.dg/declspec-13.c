/* Test declaration specifiers.  Test messages for bad type
   specifiers.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic" } */

/* typeof should act much like typedef, so the following are
   invalid.  */
typeof(double) long x0; /* { dg-error "two or more data types in declaration specifiers" } */
typeof(double) _Complex x1; /* { dg-error "two or more data types in declaration specifiers" } */

/* The following is erroneous, and used to get a bogus message about
   complex integer types.  */
typedef double D;
D _Complex x2; /* { dg-error "two or more data types in declaration specifiers" } */

/* The following empty declarations should have problems in their type
   specifiers diagnosed, not just the general problem that they are
   empty declarations.  */
long short; /* { dg-error "both 'long' and 'short' in declaration specifiers" } */
/* { dg-warning "useless type name in empty declaration" "empty" { target *-*-* } .-1 } */
_Complex double; /* { dg-warning "ISO C90 does not support complex types" } */
/* { dg-warning "useless type name in empty declaration" "empty" { target *-*-* } .-1 } */
_Complex; /* { dg-warning "ISO C90 does not support complex types" } */
/* { dg-warning "ISO C does not support plain 'complex' meaning 'double complex'" "ISO C" { target *-*-* } .-1 } */
/* { dg-warning "useless type name in empty declaration" "empty" { target *-*-* } .-2 } */
_Complex int; /* { dg-warning "ISO C90 does not support complex types" } */
/* { dg-warning "ISO C does not support complex integer types" "ISO C" { target *-*-* } .-1 } */
/* { dg-warning "useless type name in empty declaration" "empty" { target *-*-* } .-2 } */

/* Specific messages for each invalid combination.  (That some message
   is given when appropriate for a larger collection of combinations
   of type specifiers is tested in *typespec*.c.)  */

long double long x3; /* { dg-error "both 'long long' and 'double' in declaration specifiers" } */
short long x4; /* { dg-error "both 'long' and 'short' in declaration specifiers" } */
void long x5; /* { dg-error "both 'long' and 'void' in declaration specifiers" } */
_Bool long x6; /* { dg-error "both 'long' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
char long x7; /* { dg-error "both 'long' and 'char' in declaration specifiers" } */
float long x8; /* { dg-error "both 'long' and 'float' in declaration specifiers" } */
long short x9; /* { dg-error "both 'long' and 'short' in declaration specifiers" } */
void short x10; /* { dg-error "both 'short' and 'void' in declaration specifiers" } */
_Bool short x11; /* { dg-error "both 'short' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
char short x12; /* { dg-error "both 'short' and 'char' in declaration specifiers" } */
float short x13; /* { dg-error "both 'short' and 'float' in declaration specifiers" } */
double short x14; /* { dg-error "both 'short' and 'double' in declaration specifiers" } */
unsigned signed x15; /* { dg-error "both 'signed' and 'unsigned' in declaration specifiers" } */
void signed x16; /* { dg-error "both 'signed' and 'void' in declaration specifiers" } */
_Bool signed x17; /* { dg-error "both 'signed' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
float signed x18; /* { dg-error "both 'signed' and 'float' in declaration specifiers" } */
double signed x19; /* { dg-error "both 'signed' and 'double' in declaration specifiers" } */
signed unsigned x20; /* { dg-error "both 'signed' and 'unsigned' in declaration specifiers" } */
void unsigned x21; /* { dg-error "both 'unsigned' and 'void' in declaration specifiers" } */
_Bool unsigned x22; /* { dg-error "both 'unsigned' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
float unsigned x23; /* { dg-error "both 'unsigned' and 'float' in declaration specifiers" } */
double unsigned x24; /* { dg-error "both 'unsigned' and 'double' in declaration specifiers" } */
void _Complex x25; /* { dg-error "both 'complex' and 'void' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support complex types" "C90" { target *-*-* } .-1 } */
_Bool _Complex x26; /* { dg-error "both 'complex' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support complex types" "C90" { target *-*-* } .-1 } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-2 } */

long void x27; /* { dg-error "both 'long' and 'void' in declaration specifiers" } */
short void x28; /* { dg-error "both 'short' and 'void' in declaration specifiers" } */
signed void x29; /* { dg-error "both 'signed' and 'void' in declaration specifiers" } */
unsigned void x30; /* { dg-error "both 'unsigned' and 'void' in declaration specifiers" } */
_Complex void x31; /* { dg-error "both 'complex' and 'void' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support complex types" "C90" { target *-*-* } .-1 } */
/* { dg-warning "ISO C does not support plain 'complex' meaning 'double complex'" "complex" { target *-*-* } .-2 } */
long _Bool x32; /* { dg-error "both 'long' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
short _Bool x33; /* { dg-error "both 'short' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
signed _Bool x34; /* { dg-error "both 'signed' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
unsigned _Bool x35; /* { dg-error "both 'unsigned' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-1 } */
_Complex _Bool x36; /* { dg-error "both 'complex' and '_Bool' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support complex types" "C90" { target *-*-* } .-1 } */
/* { dg-warning "ISO C90 does not support boolean types" "C90" { target *-*-* } .-2 } */
/* { dg-warning "ISO C does not support plain 'complex' meaning 'double complex'" "complex" { target *-*-* } .-3 } */
long char x37; /* { dg-error "both 'long' and 'char' in declaration specifiers" } */
short char x38; /* { dg-error "both 'short' and 'char' in declaration specifiers" } */
long float x39; /* { dg-error "both 'long' and 'float' in declaration specifiers" } */
short float x40; /* { dg-error "both 'short' and 'float' in declaration specifiers" } */
signed float x41; /* { dg-error "both 'signed' and 'float' in declaration specifiers" } */
unsigned float x42; /* { dg-error "both 'unsigned' and 'float' in declaration specifiers" } */
long long double x43; /* { dg-error "both 'long long' and 'double' in declaration specifiers" } */
/* { dg-warning "ISO C90 does not support 'long long'" "C90" { target *-*-* } .-1 } */
short double x44; /* { dg-error "both 'short' and 'double' in declaration specifiers" } */
signed double x45; /* { dg-error "both 'signed' and 'double' in declaration specifiers" } */
unsigned double x46; /* { dg-error "both 'unsigned' and 'double' in declaration specifiers" } */
