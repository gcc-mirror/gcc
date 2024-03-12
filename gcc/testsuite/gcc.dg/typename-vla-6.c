/* { dg-do run } 
 * { dg-options "-std=gnu99" }
 * */



int
main (void)
{
	int a = 1;
	char tmp[2];
	if ((++a, sizeof(*((struct { char (*x)[a]; }){ &tmp }).x)) != 2)
 		__builtin_abort ();
}


