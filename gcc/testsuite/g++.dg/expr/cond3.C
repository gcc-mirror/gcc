const int i = 7;
const int j = 3;

void f(bool b) {
  &(b ? i : j);
}
