/* { dg-require-effective-target label_values } */

void callit1(void*);

extern __inline__ void test()
{
	__label__ l1;

	callit1(&&l1);

l1:;

}


void dotest()
{
	test();
}
