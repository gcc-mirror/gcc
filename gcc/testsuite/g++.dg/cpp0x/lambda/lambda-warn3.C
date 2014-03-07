// PR c++/49482
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-parameter" }

template<class T>
void f() {
  []( bool b ){ return b; };
}

int main()
{
  f<int>();
}
