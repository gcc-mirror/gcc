// { dg-do run  }
// GROUPS passed references

// execution test

int r;

const int& min(const int& tX, const int& tY)
{
        return tX < tY ? tX : tY;
}

void foo(const int m, const int n)
{
	if (m == 1 && n == 100)
	  /* OK */;
	else
	  r = 1;
}

int main()
{
        foo(min(2, 1), min(100, 200));
        return r;
}
