/* { dg-do compile } */
/* { dg-options "-W -Wall -Werror" } */

static inline int unused_fn(int dummyarg)
{
	return dummyarg*dummyarg;
}

int main()
{
	return 0;
}
