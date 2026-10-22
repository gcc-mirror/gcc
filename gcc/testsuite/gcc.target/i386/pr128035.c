/* PR target/128035 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-sse" } */
/* { dg-final { scan-assembler-not "memcmp" } } */

#define BUFSIZE 40

struct foo {
  char buf[BUFSIZE];
};

int
test (struct foo *p1, struct foo *p2)
{
  return __builtin_memcmp (p1, p2, sizeof (struct foo)) == 0;
}
