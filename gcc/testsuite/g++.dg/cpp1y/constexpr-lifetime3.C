// PR c++/70331
// { dg-do compile { target c++14 } }

constexpr int f(int i) {
  int *p = &i;
  if (i == 0) {
    int j = 123;  // { dg-message "note: declared here" }
    p = &j;
  }
  return *p;  // { dg-error "accessing 'j' outside its lifetime" }
}

constexpr int i = f(0);  // { dg-message "in .constexpr. expansion" }
