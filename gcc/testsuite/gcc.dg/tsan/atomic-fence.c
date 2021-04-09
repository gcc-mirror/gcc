/* PR sanitizer/97868 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=thread" } */

int
main ()
{
  __atomic_thread_fence (__ATOMIC_RELAXED); /* { dg-warning ".atomic_thread_fence. is not supported with .-fsanitize=thread." } */
  return 0;
}

