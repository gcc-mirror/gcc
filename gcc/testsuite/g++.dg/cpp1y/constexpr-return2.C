// { dg-do compile { target c++14 } }

constexpr int f (int i)
{
}

constexpr int i = f(42);	// { dg-error "flows off the end" }
