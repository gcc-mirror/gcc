// PR c++/84560
// { dg-do compile { target c++11 } }
// { dg-options "" }

void f() {
  int n = 1;
  int m = 1;
  int d[n][m];
  [&]() {
    return d[1];		// { dg-prune-output "sorry" }
  }();
}
