// PR c++/56915
// { dg-require-effective-target c++11 }

template <typename T>
class A
{
    typename T::type b();	// { dg-error "int" }
};

template <typename T, typename U>
void waldo(T, U) {}

template <typename T>
void bar()
{
    waldo([](A<T> a){ return a; },
          []{});
}

int main()
{
    bar<int>();
}

// { dg-prune-output "used but never defined" }
