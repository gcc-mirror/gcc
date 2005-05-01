/* Test for ICE handling internal formats: bug 20740.  The code did
   not check that, if the required typedef names had been used as
   identifiers, they were defined to suitable types.  Test
   "__gcc_host_wide_int__", not a type, asm_fprintf.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

int __gcc_host_wide_int__;
void bar (const char *, ...) __attribute__ ((__format__ (__asm_fprintf__, 1, 2))); /* { dg-error "error: '__gcc_host_wide_int__' is not defined as a type" } */
