// { dg-do run  }
class K {
public:
  int f(int i) { return i; }
};

class Q {
public:
  Q() { kp = new K; }
  int g();
private:
  K * kp;
};

int Q::g() {
  return (kp->f)(42);
}


int main () {
  Q q;
  if (q.g() != 42)
    return 1;
}
