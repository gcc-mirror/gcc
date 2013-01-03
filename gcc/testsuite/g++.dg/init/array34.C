// PR c++/53650
// We should loop over array inits if they don't involve temporaries
// that need extending.
// { dg-final { scan-assembler-times "_ZN5ClassC1Ev" 1 } }

struct Class {
  Class();
};

int main() {
  Class table [10] = {};
  return 0;
}
