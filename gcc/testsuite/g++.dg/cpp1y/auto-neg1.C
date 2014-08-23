// PR c++/60312
// { dg-do compile { target c++14 } }

template<typename> struct A;

template<> struct A<auto>	// { dg-error "auto|template argument" }
{
  template<int> void foo();
};

void bar()
{
  A<auto>().foo<0>();		// { dg-error "auto|template argument" }
}

// { dg-prune-output "expected" }
