// PR c++/106024
// { dg-do compile { target c++20 } }

void sink(...);
template <int... args> void f()
{
  sink ([] <int T> (int...) { return 1; }
        .operator()<args>(args...)...); // { dg-error "" }
} // { dg-prune-output {expected '\)'} }

int main()
{
  f<1,2,3>();
}
