/* Spurious uninitialized-variable warnings.  */
/* Disable jump threading, etc to test compiler analysis.  */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -fno-tree-dce -fno-tree-vrp -fno-tree-dominator-opts" } */

extern void use(int);
extern void foo(void);

void
func1(int cond)
{
    int x;  /* { dg-bogus "x" "uninitialized variable warning" } */

    if(cond)
	x = 1;

    foo();

    if(cond)
	use(x);
}

void
func2 (int cond)
{
    int x;  /* { dg-bogus "x" "uninitialized variable warning" } */
    int flag = 0;

    if(cond)
    {
	x = 1;
	flag = 1;
    }

    foo();

    if(flag)
	use(x);
}
