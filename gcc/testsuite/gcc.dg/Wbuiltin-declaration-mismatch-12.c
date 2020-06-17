/* Verify that declaring the __clear_cache and __builtin_prefetch
   intrinsic functions with the wrong signature is diagnosed.
   { dg-do compile }
   { dg-options "-Wbuiltin-declaration-mismatch -Wextra" } */

extern void __clear_cache (char*, char*);      // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }

void __builtin_prefetch (const char *, ...);   // { dg-warning "\\\[-Wbuiltin-declaration-mismatch" }
