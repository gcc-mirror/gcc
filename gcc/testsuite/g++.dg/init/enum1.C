enum test {
  acceptable = -1,
  unacceptable = 0xffffffffffffffffLL
}; // { dg-error "" }

enum test t = acceptable, u = unacceptable;

int main() {
    return 0;
}
