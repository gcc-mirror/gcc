// This SHOULDn't compile, becuase 'foo' is never a valid asm.

template <const unsigned c>
void f() {
        asm("foo");
}

int
main()
{
        f<1>();
}
