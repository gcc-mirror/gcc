// PR c++/49482
// { dg-options "-std=c++0x -Wunused-but-set-parameter" }

template<class T>
void f() {
  []( bool b ){ return b; };
}

int main()
{
  f<int>();
}
