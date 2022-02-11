/* { dg-options "" } */
int main() {
  int t = 1;
  unsigned c = 0, d1 = t ? 1 ^ c ^ 1 >> (-1) : 0; /* { dg-warning "is negative"  } */
  return d1;
}
