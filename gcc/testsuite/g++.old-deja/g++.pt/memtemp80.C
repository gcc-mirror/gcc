// Build don't link:

template<typename T> T baz() { return 0; }

struct foo {
       template<typename T> static T staticbar() { return 0; }
       template<typename T> T bar() { return 0; }
};

void f()
{
       foo t;
       int i = baz<int>();
       int j = foo::staticbar<int>();
       int k = t.bar<int>();
}
