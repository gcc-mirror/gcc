// { dg-do assemble  }

char *a="aê";

class A
{
public:
	A()
	{
		char *b="aê";
	}
};

char *c="aê";
