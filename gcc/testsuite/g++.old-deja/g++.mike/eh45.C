// { dg-do run  }
int i;
int fail;

class ca {
public:
  ca() {
    if (++i != 1)
      fail = 1;
  }
  virtual ~ca() {
    if (++i != 4)
      fail = 4;
  }
};

class cb {
public:
  cb(const ca &rca) {
    if (++i != 2)
      fail = 2;
  }
  virtual ~cb() {
    if (++i != 3)
      fail = 3;
  }
};

void foo(const cb &rcb) {
}

int main(int argc, char **argv) {
  foo(cb(ca()));
  return fail;
}
