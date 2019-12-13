// PR c++/24667
// { dg-options "-Wcast-qual" }

int main(int, char**) {
  const int foo[2] = {1,1};
  ((int*)foo)[0] = 0; // { dg-warning "4:cast" }
}
