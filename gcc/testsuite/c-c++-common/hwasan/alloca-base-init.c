/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-additional-options "--param hwasan-random-frame-tag=1" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
#include <alloca.h>

/* This testcase checks that `alloca` calls ensure the `__hwasan_generate_tag`
   function is called to initialize the base tag.  `alloca` calls are treated
   differently to standard variables.  The prologue/epilogue sequence is
   generated mainly based on normal stack-allocated objects.

   We want to ensure that though the `alloca` call is not poisoned/unpoisoned
   by the prologue and epilogue, the use of them in a given function still
   triggers the prologue sequence to emit a call to __hwasan_generate_tag (and
   hence that any call to __hwasan_generate_tag is emitted in the unconditional
   part of the function code).  */

int choice = 0;
int record = 1;

#ifdef __cplusplus
extern "C" {
#endif
__attribute__ ((noinline))
unsigned char
__hwasan_generate_tag ()
{
  record = 0;
  return 3;
}
#ifdef __cplusplus
}
#endif

__attribute__ ((noinline))
int
generate_tag_was_missed (void)
{
  return record;
}

__attribute__((noinline, noclone)) int
foo (char *a)
{
  int i, j = 0;
  asm volatile ("" : "+r" (a) : : "memory");
  for (i = 0; i < 12; i++)
    j += a[i];
  return j;
}

int
main ()
{
  if (choice)
  {
        char *x = (char *)alloca(100);
        foo(x);
  }
  else
  {
        char *y = (char *)alloca(20);
        foo(y);
  }
  return generate_tag_was_missed ();
}
