// { dg-do run  }
// prms-id: 3579

extern "C" int printf(const char *, ...);

int num_x;

class Y {
public:
  Y () { printf("Y()            this: %x\n", this); }
  ~Y () { printf("~Y()           this: %x\n", this); }
};

class X {
public:
  X () {
    ++num_x;
    printf("X()            this: %x\n", this);
    Y y;
    *this = (X) y;
  }

  X (const Y & yy) { printf("X(const Y&)    this: %x\n", this); ++num_x; }
  X & operator = (const X & xx) {
    printf("X.op=(X&)      this: %x\n", this);
    return *this;
  }

  ~X () { printf("~X()           this: %x\n", this); --num_x; }
};

int main (int, char **) {
    { X anX; }
    if (num_x) {
      printf("FAIL\n");
      return 1;
    }
    printf("PASS\n");
    return 0;
}
