/* { dg-do compile } */

#include <setjmp.h>

extern int optind;
jmp_buf jump_buf;
int
main (int argc, char **argv)
{
  foo (jump_buf, setjmp(jump_buf));
  argv += optind;
  bar(argv[1]);
}
