/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic -g" } */
/* { dg-additional-options "-m31" { target s390x-*-* } } */

typedef __SIZE_TYPE__ uintptr_t;
static __thread uintptr_t start_sp;
static inline uintptr_t
__thread_stack_pointer (void)
{
  return (uintptr_t) __builtin_frame_address (0);
}

void
update_data (void)
{
  if (__builtin_expect ((!start_sp), 0))
    start_sp = __thread_stack_pointer ();

  uintptr_t sp = __thread_stack_pointer ();
  if (__builtin_expect ((sp > start_sp), 0))
    start_sp = sp;
}
