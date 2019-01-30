// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/10583: ICE using template function with invalid signature.

template <typename> 
struct A 
{
    struct B 
    {};
};

template <typename T> 
void func(A<T>::B* )	// { dg-error "variable|template|expression" }
{
}

int main() 
{
  func<void>(0);	// { dg-error "not declared|expression|;" }
}
