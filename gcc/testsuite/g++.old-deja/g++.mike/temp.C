extern "C" int printf(const char *, ...);
extern "C" const char *getenv(const char *);

class T {
  int i;
public:
  T() {
    i = 1;
    printf("T() at %x\n", this);
  }
  T(const T& o) {
    i = o.i;
    printf("T(const T&) at %x <-- %x\n", this, &o);
  }
  T operator +(const T& o) {
    T r;
    r.i = this->i + o.i;
    return r;
  }
  operator int () {
    return i;
  }
  ~T() { printf("~T() at %x\n", this); }
} s, b;

int foo() { return getenv("TEST") == 0; }

int main() {
  int i = foo() ? s+b : s;
  return i != 2;
}
