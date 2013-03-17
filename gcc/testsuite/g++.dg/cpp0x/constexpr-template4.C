// PR c++/55931
// { dg-do compile { target c++11 } }

#include <type_traits>

template<typename Type>
class Test
{
    public:
        constexpr Test(const Type val) : _value(val) {}
        constexpr Type get() const {return _value;}
        static void test()
        {
            static constexpr Test<int> x(42);
            std::integral_constant<int, x.get()> i; // This is not working
        }
    protected:
        Type _value;
};

int main()
{
    static constexpr Test<int> x(42);
    std::integral_constant<int, x.get()> i; // This is working
    Test<double>::test();
}
