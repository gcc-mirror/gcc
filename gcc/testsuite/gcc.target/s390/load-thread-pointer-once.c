/* { dg-do compile } */
/* { dg-options "-O3" } */

__thread void *foo;

void *bar()
{
  return (foo = __builtin_thread_pointer());
}

/* { dg-final { scan-assembler-times {\n\tear\t} 2 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times {\n\tear\t} 1 { target { ! lp64 } } } } */
