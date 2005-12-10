// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }
// { dg-options -w }

struct X {
  char : 45;
};

int main () {
  if (__alignof__ (X) != 4)
    return 1;
}
