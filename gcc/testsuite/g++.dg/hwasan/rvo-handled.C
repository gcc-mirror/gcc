/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-skip-if "" { *-*-* }  { "-O0" } { "" } } */

#define assert(x) if (!(x)) __builtin_abort ()
typedef __UINTPTR_TYPE__ uintptr_t;
void *untagged (void *ptr)
{
  /* Untag by removing the top byte.  */
  return (void*)((uintptr_t)ptr & 0xffffffffffffff);
}

struct big_struct {
    int left;
    int right;
    void *ptr;
    int big_array[100];
};

/*
   Tests for RVO (basically, checking -fsanitize=hwaddress has not broken RVO
   in any way).

   0) The value is accessible in both functions without a hwasan complaint.
   1) RVO does happen.
 */

struct big_struct __attribute__ ((noinline))
return_on_stack()
{
  struct big_struct x;
  x.left = 100;
  x.right = 20;
  x.big_array[10] = 30;
  x.ptr = untagged(&x);
  return x;
}

int main()
{
  struct big_struct x;
  x = return_on_stack();
  /* Check that RVO happens by checking the address that the callee saw.  */
  assert (x.ptr == untagged(&x));
  return 0;
}
