// Build don't link:
// excess errors test

struct foo {
       template<typename T> T bar() { return staticbar<T>( this ); }
       template<typename T> static T staticbar( foo* ) { return 0; }
};

void f()
{
       foo t;
       int k = t.bar<int>();
}
