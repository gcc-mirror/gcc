// PR c++/77945
// { dg-do compile { target c++11 } }

struct T 
{ 
    int x = 0; 
    bool y = 0; 
    constexpr T() {}
};

int main()
{
    constexpr T t = (T{} = T{}); // { dg-error "" "" { target c++11_only } }
}
