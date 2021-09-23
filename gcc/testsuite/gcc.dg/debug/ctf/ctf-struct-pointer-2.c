/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* This tests the following scenario:

   1. struct foo;
   2. struct foo *a_foo;
   3. struct foo { int bar; };
   4. void baz (struct foo **f) { f->bar = 0; }

   At 2. a forward for struct foo is generated and at 3. the struct
   type is fully defined.  When a pointer to a pointer to foo is
   encountered at 4., an additional CTF type for the completed struct
   shall be emitted as well.  The linker will deduplicate both
   types.  */

struct foo;
struct foo *a_foo;
struct foo { int bar; };
void baz (struct foo **f) { (*f)->bar = 0; }

/* { dg-final { scan-assembler-times "\[\t \]\"bar.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
