// Test for range-based for loop error in C++98 mode

// { dg-do compile { target { ! c++11 } } }

void test()
{
    int a[] = {0,1,2};
    for (int x : a)  // { dg-error "range-based 'for'|forming reference" }
        ;
}
