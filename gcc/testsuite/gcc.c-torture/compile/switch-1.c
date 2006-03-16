/* PR middle-end/26557.  */
const int struct_test[1] = {1};
void g();
void f() {
  switch(struct_test[0]) {
    case 1: g();
  }
}

