// Build don't link:
// Origin: "Weidmann, Nicholas" <nicholas.weidmann@swx.ch>
// Skip if not target: i?86-*-linux*

template<int i> int foo(int v)
{
	__asm__ __volatile__("addl %1, %0" : "=a" (v) : "b" (i));

	return v;
}

int bar(int i)
{
	return foo<123>(i);
}

