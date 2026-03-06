// PR c++/124297
// { dg-do compile { target c++20 } }

template<class T> concept C = true;

template<C auto... Vs> void f();

int main() {
  f<42>();
}

// { dg-final { scan-assembler "_Z1fITpTnDk1CJLi42EEEvv" } }
