/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-sse" } */
/* { dg-final { scan-assembler-not "jmp\tmemset" } } */
/* { dg-final { scan-assembler-not "rep stosb" } } */

struct foo
{
  char buf[41];
};

void
zero(struct foo *f)
{
  __builtin_memset(f->buf, 0, sizeof(f->buf));
}
