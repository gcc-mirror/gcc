/* Test for weak aliases with multiple declarations.  Sun assembler
   rejects multiple weak alias definitions in the output.  */
/* { dg-do assemble } */
/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-options "" } */

#pragma weak foo = _foo

extern int foo;
extern int foo;

int _foo = 4;
