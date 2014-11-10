/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

extern int optind;
jmp_buf jump_buf;
void foo (jmp_buf, int);
void bar (char *);
int
main (int argc, char **argv)
{
  foo (jump_buf, setjmp(jump_buf));
  argv += optind;
  bar(argv[1]);
}
