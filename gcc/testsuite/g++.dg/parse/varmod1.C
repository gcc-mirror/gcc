int main(int argc, char** argv) {
  int nx = 2;
  void theerror(double a[][nx+1]); // { dg-message "" }
  double** a;
  theerror(a);
  return 0;
}
