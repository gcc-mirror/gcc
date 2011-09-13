/* PR middle-end/50266 */
/* Testcase by <bero@arklinux.org> */

struct a {
 unsigned int a;
 unsigned int b;
};

struct a *const p = (struct a *)0x4A004100;

void foo(void)
{
 unsigned int i = 0;
 unsigned int *const x[] = {
  &p->a,
  &p->b,
  0
 };

 (*(volatile unsigned int *)((x[i]))
   = (unsigned int)((unsigned int)((*(volatile unsigned int *)(x[i])))));
}
