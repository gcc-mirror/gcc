int main() {
  int i = 2;
  int *pi = &(++i);

  return i != 3;
}
