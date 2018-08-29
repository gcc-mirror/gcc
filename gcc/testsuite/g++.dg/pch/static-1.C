#include "static-1.H"
int LocalStaticTest()
{
        static A sa;
	return 0;
}

int main(int argc, char **argv)
{
        A::StaticTest();
	return 0;
}
