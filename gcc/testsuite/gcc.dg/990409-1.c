/* Test that __LINE__ works when embedded in a macro. */
/* { dg-do run } */

#define foo() bar(__LINE__)

void
bar(int x)
{
    if (x != 16)
	abort();
}

int
main(void)
{
    foo();    /* This is line 16 */
    return 0;
}
