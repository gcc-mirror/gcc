// PR c++/78840
// { dg-do compile { target c++14 } }

int global;

void Bar (int);

template<int I> void Foo () {
  const int cst = global;
  auto lam0 = [&]() -> void {
    auto lam1 = [&]() -> void { cst; };
    
    Bar (cst);
  };
}

template void Foo <0> ();
