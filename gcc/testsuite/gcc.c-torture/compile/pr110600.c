int a(int b, int c) { return (b ^ c) < 0 ? b : b - c; }
int main() {
  for (int e = 0; e != -1; e = a(e, 1))
    ;
  return 0;
}
