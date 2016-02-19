/* Machine description pattern tests.  */

/* { dg-do run } */
/* { dg-options "-dP -save-temps" } */

__attribute__ ((noinline))
void test(char *dest, const char *src)
{
  __builtin_stpcpy (dest, src);
}

/* { dg-final { scan-assembler-times {{[*]movstr}|{vec_vfenesv16qi}} 1 } } */

#define LEN 200
char buf[LEN];

int main(void)
{
  __builtin_memset(buf, 0, LEN);
  test(buf, "hello world!");
  if (__builtin_strcmp(buf, "hello world!") != 0)
    __builtin_abort();
  return 0;
}
