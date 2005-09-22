// PR c++/23993

const int data[2][4] = {
  { 0, 1, 2, 3 }
};

template <typename T>
void t(int k) {
  int candidate = data[1][k];
}
