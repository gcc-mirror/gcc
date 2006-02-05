// { dg-do assemble  }

const char *a="aê";

class A
{
public:
	A()
	{
		const char *b="aê";
	}
};

const char *c="aê";
