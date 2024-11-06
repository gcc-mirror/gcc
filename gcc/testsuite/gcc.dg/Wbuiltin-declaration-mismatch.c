/* PR c/83656 - missing -Wbuiltin-declaration-mismatch on declaration
   without prototype
   { dg-do compile }
   { dg-options "-std=gnu17 -Wall" } */

typedef __SIZE_TYPE__ size_t;

/* Verify that ordinary library built-ins are not diagnosed with -Wall
   (or by default) whether or not they take arguments (even though they
   should be).  */

void abort ();
void* memcpy ();
void* memset ();
size_t strlen ();

/* Verify mismatches in return types are diagnosed.  */
int exit ();        /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" } */

/* Variadic built-ins are diagnosed with -Wall (they are, in fact,
   diagnosed by default).  */
int printf ();      /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" } */
int sprintf ();     /* { dg-warning "\\\[-Wbuiltin-declaration-mismatch]" } */
