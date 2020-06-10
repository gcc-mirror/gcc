/* PR rtl-optimization/51447 */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */

extern void abort (void);

#ifdef __x86_64__
register void *ptr asm ("rbx");
#else
void *ptr;
#endif

int
main (void)
{
  __label__ nonlocal_lab;
#ifdef __x86_64__
  void *save = ptr;
#endif
  __attribute__((noinline, noclone)) void
    bar (void *func)
      {
	ptr = func;
	goto nonlocal_lab;
      }
  bar (&&nonlocal_lab);
  return 1;
nonlocal_lab:
  if (ptr != &&nonlocal_lab)
    abort ();
#ifdef __x86_64__
  ptr = save; /* Restore the call-saved register.  */
#endif
  return 0;
}
