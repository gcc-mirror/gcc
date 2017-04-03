/* Machine description pattern tests.  */

/* { dg-do compile } */
/* { dg-options "-mmvcle -dP -save-temps" } */
/* { dg-do run { target { s390_useable_hw } } } */

/* Skip test if -O0 is present on the command line or -O... is missing:

    { dg-skip-if "" { *-*-* } { "-march=z9*" "-O0" } { "" } }
    { dg-skip-if "" { *-*-* } { "*" } { "-O*" } }
*/

__attribute__ ((noinline))
void test(char *p, char c, int len)
{
  __builtin_memset(p, c, len);
}

__attribute__ ((noinline))
void test2(char *p, int c, int len)
{
  __builtin_memset(p, (char)c, len);
}

/* Check that the right patterns are used.  */
/* { dg-final { scan-assembler-times {c"?:16 .*{[*]setmem_long_?3?1?z?}} 1 } } */
/* { dg-final { scan-assembler-times {c"?:22 .*{[*]setmem_long_and_?3?1?z?}} 1 } } */

#define LEN 500
char buf[LEN + 2];

void init_buf(void)
{
  int i;

  buf[0] = 0;
  for (i = 1; i <= LEN; i++)
    buf[i] = (0x10 + (i & 0x3f));
  buf[LEN + 1] = 0x7f;
}

void validate_buf(char val)
{
  int i;

  if (buf[0] != 0)
    __builtin_abort();
  for (i = 1; i <= LEN; i++)
    if (buf[i] != val)
      __builtin_abort();
  if (buf[LEN + 1] != 0x7f)
    __builtin_abort();
}

int main(void)
{
  init_buf();
  test(buf + 1, 55, LEN);
  validate_buf(55);
  init_buf();
  test(buf + 1, 66, LEN);
  validate_buf(66);
}
