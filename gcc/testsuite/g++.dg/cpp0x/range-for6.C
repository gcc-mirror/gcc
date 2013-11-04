// Test for range-based for loop
// Test the loop with an initializer_list

// { dg-do run }
// { dg-options "-std=c++11" }

#include <initializer_list>

extern "C" void abort();

template<typename T> T foo()
{
    T sum = 0;
    for (T x : {T(1),T(2),T(3),T(4)})
        sum += x;
    if (sum != T(10))
        abort();
}

int main()
{
    int sum = 0;
    for (int x : {1,2,3,4})
        sum += x;
    if (sum != 10)
        abort();

    foo<int>();
}
