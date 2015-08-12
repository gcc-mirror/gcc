// PR c++/65843
// { dg-do compile { target c++11 } }

template<class T>
void test(T b)
{
    const int a = b;
    [&] () { return a, a; }();
}

int main() {
    test(1);
 return 0;
}
