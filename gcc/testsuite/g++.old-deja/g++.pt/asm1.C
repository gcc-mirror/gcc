// { dg-do assemble { target i?86-*-linux* x86_64-*-linux* } }
// We'd use ebx with -fpic/-fPIC, so skip.
// { dg-skip-if "" { i?86-*-* } { "-fpic" "-fPIC" } { "" } }
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

