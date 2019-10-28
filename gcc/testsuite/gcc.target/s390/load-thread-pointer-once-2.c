/* { dg-do compile } */
/* { dg-options "-O3" } */

extern void c(void *);

void a(void)
{
  void *b = __builtin_thread_pointer();
  if (b)
    c(b);
}

/* { dg-final { scan-assembler-times {\n\tear\t} 2 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times {\n\tear\t} 1 { target { ! lp64 } } } } */
