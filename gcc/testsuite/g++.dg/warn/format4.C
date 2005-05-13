// { dg-do compile }
// { dg-options "-Wformat=2" }

extern "C" int printf (const char*, ...);

void foo(int i)
{
	printf("Hello World %d!\n", i);
	printf(&"Hello World %d!\n"[0], i);
	printf(&"Hello World %d!\n"[6], i);
	printf(&"Hello World %d!\n"[8]-2, i);
	printf(&"Hello World %d!\n"[4]+2, i);
}
