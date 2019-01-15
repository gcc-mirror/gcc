/* Test for format attributes: test bad uses of __attribute__.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#define DONT_GNU_PROTOTYPE
#include "format.h"

/* Proper uses of the attributes.  */
extern void fa0 (const char *, ...) __attribute__((format(gnu_attr_printf, 1, 2)));
extern void fa1 (char *, ...) __attribute__((format(gnu_attr_printf, 1, 2)));
extern char *fa2 (const char *) __attribute__((format_arg(1)));
extern char *fa3 (char *) __attribute__((format_arg(1)));

/* Uses with too few or too many arguments.  */
extern void fb0 (const char *, ...) __attribute__((format)); /* { dg-error "wrong number of arguments" "bad format" } */
extern void fb1 (const char *, ...) __attribute__((format())); /* { dg-error "wrong number of arguments" "bad format" } */
extern void fb2 (const char *, ...) __attribute__((format(gnu_attr_printf))); /* { dg-error "wrong number of arguments" "bad format" } */
extern void fb3 (const char *, ...) __attribute__((format(gnu_attr_printf, 1))); /* { dg-error "wrong number of arguments" "bad format" } */
extern void fb4 (const char *, ...) __attribute__((format(gnu_attr_printf, 1, 2, 3))); /* { dg-error "wrong number of arguments" "bad format" } */

extern void fc1 (const char *) __attribute__((format_arg)); /* { dg-error "wrong number of arguments" "bad format_arg" } */
extern void fc2 (const char *) __attribute__((format_arg())); /* { dg-error "wrong number of arguments" "bad format_arg" } */
extern void fc3 (const char *) __attribute__((format_arg(1, 2))); /* { dg-error "wrong number of arguments" "bad format_arg" } */

/* These attributes presently only apply to declarations, not to types.
   Eventually, they should be usable with declarators for function types
   anywhere, but still not with structure/union/enum types.  */
struct s0 { int i; } __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error "does not apply|only applies" "format on struct" } */
union u0 { int i; } __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error "does not apply|only applies" "format on union" } */
enum e0 { E0V0 } __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error "does not apply|only applies" "format on enum" } */

struct s1 { int i; } __attribute__((format_arg(1))); /* { dg-error "does not apply|only applies" "format_arg on struct" } */
union u1 { int i; } __attribute__((format_arg(1))); /* { dg-error "does not apply|only applies" "format_arg on union" } */
enum e1 { E1V0 } __attribute__((format_arg(1))); /* { dg-error "does not apply|only applies" "format_arg on enum" } */

/* The format type must be an identifier, one of those recognized.  */
extern void fe0 (const char *, ...) __attribute__((format(12345, 1, 2))); /* { dg-error "format specifier" "non-id format" } */
extern void fe1 (const char *, ...) __attribute__((format(nosuch, 1, 2))); /* { dg-warning "format function type" "unknown format" } */

/* Both the numbers must be integer constant expressions.  */
extern void ff0 (const char *, ...) __attribute__((format(gnu_attr_printf, 3-2, (long long)(10/5))));
int foo;
extern void ff1 (const char *, ...) __attribute__((format(gnu_attr_printf, foo, 10/5))); /* { dg-warning ".format. attribute argument 2 value .foo. is not an integer constant" "bad number" } */
extern void ff2 (const char *, ...) __attribute__((format(gnu_attr_printf, 3-2, foo))); /* { dg-warning ".format. attribute argument 3 value .foo. is not an integer constant" "bad number" } */
extern char *ff3 (const char *) __attribute__((format_arg(3-2)));
extern char *ff4 (const char *) __attribute__((format_arg(foo))); /* { dg-warning ".format_arg. attribute argument value .foo. is not an integer constant" "bad format_arg number" } */

/* The format string argument must precede the arguments to be formatted.
   This includes if no parameter types are specified (which is not valid ISO
   C for variadic functions).  */
extern void fg0 () __attribute__((format(gnu_attr_printf, 1, 2)));
extern void fg1 () __attribute__((format(gnu_attr_printf, 1, 0)));
extern void fg2 () __attribute__((format(gnu_attr_printf, 1, 1))); /* { dg-error "follows" "bad number order" } */
extern void fg3 () __attribute__((format(gnu_attr_printf, 2, 1))); /* { dg-error "follows" "bad number order" } */

/* The format string argument must be a string type, and the arguments to
   be formatted must be the "...".  */
extern void fh0 (int, ...) __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error ".format. attribute argument 2 value .1. refers to parameter type .int." "format int string" } */
extern void fh1 (signed char *, ...) __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error ".format. attribute argument 2 value .1. refers to parameter type .signed char \\\*." "signed char string" } */
extern void fh2 (unsigned char *, ...) __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error ".format. attribute argument 2 value .1. refers to parameter type .unsigned char \\\*." "unsigned char string" } */
extern void fh3 (const char *, ...) __attribute__((format(gnu_attr_printf, 1, 3))); /* { dg-error "is not" "not ..." } */
extern void fh4 (const char *, int, ...) __attribute__((format(gnu_attr_printf, 1, 2))); /* { dg-error ".format. attribute argument 3 value .2. does not refer to a variable argument list" "not ..." } */

/* format_arg formats must take and return a string.  */
extern char *fi0 (int) __attribute__((format_arg(1))); /* { dg-error ".format_arg. attribute argument value .1. refers to parameter type .int." } */
extern char *fi1 (signed char *) __attribute__((format_arg(1))); /* { dg-error ".format_arg. attribute argument value .1. refers to parameter type .signed char \\\*." "format_arg signed char string" } */
extern char *fi2 (unsigned char *) __attribute__((format_arg(1))); /* { dg-error ".format_arg. attribute argument value .1. refers to parameter type .unsigned char \\\*." "format_arg unsigned char string" } */
extern int fi3 (const char *) __attribute__((format_arg(1))); /* { dg-error "not return string" "format_arg ret int string" } */
extern signed char *fi4 (const char *) __attribute__((format_arg(1))); /* { dg-error "not return string" "format_arg ret signed char string" } */
extern unsigned char *fi5 (const char *) __attribute__((format_arg(1))); /* { dg-error "not return string" "format_arg ret unsigned char string" } */
