import core.stdc.stdio;

extern (C) int main()
{
    printf("hello world\n");
    foo(3);
    return 0;
}

int foo(int i)
{
    final switch (i)
    {
	case 1: break;
    }
    return i;
}
