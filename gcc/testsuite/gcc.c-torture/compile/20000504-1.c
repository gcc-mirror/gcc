struct foo {
	int a;
	int b;
};

int func(struct foo *foo, int a)
{
	if (foo->b == 0) {
		int ret = foo->a = a;
		if (a >= 0)
			foo->a = a;
		return (ret);
 	}
	return (0);
}
