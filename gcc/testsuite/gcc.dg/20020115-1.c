/* { dg-do compile } */

/* Test attributes in function arguments.  */
/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */

#define blah __attribute__((__mode__(QI)))

extern void bar(int *);

void foo (blah int abc)
{

	int b[sizeof(abc) == 1 ? 1 : -1];
	bar (b);
}
