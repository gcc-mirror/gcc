void f(int &);
void f(const int &);
int main() {
  volatile int x = 2;
  f((int)x);
}
