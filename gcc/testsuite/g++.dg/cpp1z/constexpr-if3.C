// Testcase from P0292R2
// { dg-do link { target c++11 } }
// { dg-options "" }

extern int x;   // no definition of x required
int main() {
  if constexpr (true) // { dg-warning "constexpr" "" { target c++14_down } }
    return 0;
  else if (x)
    return x;
  else
    return -x;
}
