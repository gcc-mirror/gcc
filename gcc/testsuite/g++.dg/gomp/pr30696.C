inline void foo() {}

int main()
{
    foo();

#pragma omp parallel for
    for ( int i=0; i<1; ++i )
        foo();

    return 0;
}
