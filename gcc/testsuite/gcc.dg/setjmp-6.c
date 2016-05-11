/* PR69569 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

jmp_buf buf;

struct str {
    int Count;
};
int fun2(struct str *p1)
{
    int i = 1;
    while (1) {
	setjmp(buf);
	break;
    }
    for (; i;) {
	i = 0;
	for (; i < (p1 ? p1->Count : 1); i++)
	  fun2(p1);
    }
    return 1;
}
