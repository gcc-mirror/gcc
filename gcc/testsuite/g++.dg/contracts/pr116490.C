// ICE in explicit instantiation of a function with contracts
// { dg-do run }
// { dg-options "-std=c++20 -fcontracts -fcontract-continuation-mode=on" }

template<class T>
void foo(T t)
[[pre : t == 9 ]] {
}

template void foo<int>(int i);


template<class T>
struct templateS
{
  void fooS(T t)
  [[pre : t == 9 ]] {
  }
};

template struct templateS<int>;


struct S {

  template<class T>
  void fooS(T t)
  [[pre : t == 9 ]] {
  }

  template<class T>
  static void fooStatic(T t)
  [[pre : t == 9 ]] {
  }
};

template void S::fooS<int>(int i);

template void S::fooStatic<int>(int i);

int main()
{
   foo(3);

   templateS<int> ts;
   ts.fooS(3);

   S s;
   s.fooS(3);
   S::fooStatic(3);
}

// { dg-output "contract violation in function foo<int> at .* t == 9.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function templateS<int>::fooS at .* t == 9.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::fooS<int> at .* t == 9.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::fooStatic<int> at .* t == 9.*(\n|\r\n|\r)" }
