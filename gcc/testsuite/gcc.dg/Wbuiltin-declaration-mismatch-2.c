/* PR c/83656 - missing -Wbuiltin-declaration-mismatch on declaration
   without prototype
   { dg-do compile }
   { dg-options "-Wall -Wextra" } */

typedef __SIZE_TYPE__ size_t;

/* Verify that ordinary library built-ins are not diagnosed with -Wextra
   when they take no arguments (except in cases of return type mismatches).
   This is in anticipation that C may some day adopt the same syntax as
   C++ for declaring functions that take no arguments.  */

void abort ();

/* Verify that ordinary library built-ins are diagnosed with -Wextra
   when they take arguments.  */

void* memcpy ();    /* { dg-warning "declaration of built-in function .memcpy. without a prototype; expected .void \\\*\\\(void \\\*, const void \\\*, \(long \)*unsigned int\\\)." } */
void* memset ();    /* { dg-warning "declaration of built-in function .memset. without a prototype; expected .void \\\*\\\(void \\\*, int, *\(long \)*unsigned int\\\)." } */
size_t strlen ();   /* { dg-warning "declaration of built-in function .strlen. without a prototype; expected .\(long \)*unsigned int\\\(const char \\\*\\\)." } */

/* Variadic built-ins are diagnosed even without -Wextra (they are,
   in fact, diagnosed by default).  */
int printf ();      /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" } */
int sprintf ();     /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" } */
