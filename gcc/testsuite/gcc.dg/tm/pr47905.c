/* { dg-do compile }
   { dg-options "-fgnu-tm" } */

void funcA();
void funcB();

void *thread()
{
	__transaction_relaxed
	{
		funcA();
	};
	funcB();
}
