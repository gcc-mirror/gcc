#include "static-1.H"
int LocalStaticTest()
{
        static A sa;
}

int main(int argc, char **argv)
{
        A::StaticTest();
}
