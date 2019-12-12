/* Verify that declaring the __clear_cache and __builtin_prefetch
   intrinsic functions with the wrong signature is diagnosed.
   { dg-do compile }
   { dg-options "-Wbuiltin-declaration-mismatch -Wextra" } */

extern void __clear_cache (char*, char*);   /* { dg-warning "mismatch in argument 1 type of built-in function .__clear_cache.; expected .void \\\*." } */

void __builtin_prefetch (const char *, ...);   /* { dg-warning "mismatch in argument 1 type of built-in function .__builtin_prefetch.; expected .const void \\\*." } */
