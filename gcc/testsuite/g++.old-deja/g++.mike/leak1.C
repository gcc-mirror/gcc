int count = 0;

class T {
  int i;
public:
  T() {
    i = 1;
    ++count;
  }
  T(const T& o) {
    i = o.i;
    ++count;
  }
  T operator +(const T& o) {
    T r;
    r.i = this->i + o.i;
    return r;
  }
  operator int () {
    return i;
  }
  ~T() {
    --count;
  }
} s, b;

void bar() {
  static int j = int(s+b);
  int i = int(s+b);
}

int i = int(s+b);

int main() {
  bar();
  bar();
  return count != 2;
}
