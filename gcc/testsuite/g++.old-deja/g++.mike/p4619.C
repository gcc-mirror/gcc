// Build don't link:
// prms-id: 4619

int main() {
  int i = 3;
  int (*p)[10] = new int [20][10];
  int (*p1)[5][7][13][10] = new int [i][5][7][13][10];
  delete [] p1;
  delete [] p;
}
