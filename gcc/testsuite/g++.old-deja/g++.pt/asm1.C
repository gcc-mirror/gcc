// { dg-do assemble { target i?86-*-linux* x86_64-*-linux* } }
// We'd use ebx with 32-bit pic code, so skip.
// { dg-skip-if "" { ilp32 && { ! nonpic } } { "*" } { "" } }
// Origin: "Weidmann, Nicholas" <nicholas.weidmann@swx.ch>

template<int i> int foo(int v)
{
	__asm__ __volatile__("addl %1, %0" : "=a" (v) : "b" (i));

	return v;
}

int bar(int i)
{
	return foo<123>(i);
}

