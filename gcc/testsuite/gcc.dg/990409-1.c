/* Test that __LINE__ works when embedded in a macro. */
/* { dg-do run } */

#define XLINE __LINE__

void
bar(int x, int y)
{
    if (x != y)
	abort();
}

int
main(void)
{
    bar(XLINE, __LINE__);
    return 0;
}
