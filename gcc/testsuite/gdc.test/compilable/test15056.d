nothrow:

version (Windows)
{
    version (LP_64)
        import core.stdc.stdlib;
    else
        // doesn't currently work b/c SEH remains present even in nothrow code
        void* alloca(size_t) { return null; }
}
else
    import core.stdc.stdlib;

struct S
{
	~this() nothrow {}
}

S foo(void* p = alloca(1234))
{
	return S();
}

int main()
{
	foo();
	return 0;
}
