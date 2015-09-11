// { dg-do compile }
// { dg-require-effective-target named_sections }
 class A
{
	public:
		int __attribute__((section("a"))) f1(bool);
		int f2(void *);
		int f3(bool);
};

inline int A::f1(bool b)
{
	static  int c;
	if (c)
		;
	return 0;
}

inline int A::f3(bool b)
{
	static __attribute__((section(""))) int c;
	if (c)
		;
	return 0;
}

int A::f2(void *c)
{
	return f1(c) + f3(c);
}
