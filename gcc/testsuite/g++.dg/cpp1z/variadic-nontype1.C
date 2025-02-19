// PR c++/118265
// { dg-do compile { target c++17 } }

struct Class1
{
    void apply_bool(bool){}
    void apply_char(char){}
};

template<auto...Fn> struct Class2;
template<typename...P, void(Class1::*...Fn)(P)> struct Class2<Fn...>
{
    void apply(){}
};

int main()
{
    Class2<&Class1::apply_bool, &Class1::apply_char> class2;
    class2.apply ();
}
