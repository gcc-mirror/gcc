// { dg-do run }
// Copyright (C) 2002 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Incorrect construction and destruction of multi-dimensional
// array of class.

extern "C" void abort();
extern "C" int printf(const char *, ...);

int count;
int num;

struct A
{
	A()
	{
		if (count == num)
			throw "";
		count++;
#ifdef PRINT
		printf("ctor %p\n", static_cast<void *>(this));
#endif
	}

	~A()
	{
		count--;
#ifdef PRINT
		printf("dtor %p\n", static_cast<void *>(this));
#endif
	}
};

struct Array
{
	A array[2][2][2];
};

int main()
{
	for (num = 0; num <= 8; ++num) {
		count = 0;
		try {
			Array A;
		}
		catch (...) {
		}
		if (count != 0)
			abort();
	}
}
