// PR c++/23172
// { dg-do run }
// { dg-options "" }

int i = (int) {7};

int main () {
  if (i != 7)
    return 1;
}
