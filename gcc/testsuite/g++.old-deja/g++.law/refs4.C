// GROUPS passed references
extern "C" int printf (const char *, ...);

const int& min(const int& tX, const int& tY)
{
        return tX < tY ? tX : tY;
}

void foo(const int m, const int n)
{
	if (m == 1 && n == 100)
	  printf("PASS\n");
	else
	  printf("FAIL\n");
}

int main()
{
        foo(min(2, 1), min(100, 200));
        return 0;
}
