/* 920730-1.c */

f1()
{
	int b=0x80000000;
	return b>=0x80000000;
}

f2()
{
	int b=0x80000001;
	return b>=0x80000001;
}

f3()
{
	int b=0x7fffffff;
	return b>=0x7fffffff;
}

f4()
{
	int b=0xffffffff;
	return b>=0xffffffff;
}

main ()
{
	if((f1()&f2()&f3()&f4())!=1)
		abort();
		exit(0);
}
