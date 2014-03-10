// Test for range-based for loop
// Test the loop with an array

// { dg-do run { target c++11 } }

extern "C" void abort();

int main()
{
    int a[] = {1,2,3,4};
    int sum = 0;
    for (int x : a)
        sum += x;
    if (sum != 10)
        abort();
}
