/* PR middle-end/94423 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=object-size" } */

void foo (void);
typedef struct { long buf[22]; } jmp_buf[1];
extern int sigsetjmp (jmp_buf, int) __attribute__ ((__nothrow__));
jmp_buf buf;

void
bar (int *c)
{
  while (*c)
    foo ();
  while (*c)
    sigsetjmp (buf, 0);
}
