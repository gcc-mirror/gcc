__gshared private:
    int j;
    extern(C++, ns) int k;

void f()
{
    j = 0; // works as expected
    k = 0; // Error: variable foo.ns.k is private
}
