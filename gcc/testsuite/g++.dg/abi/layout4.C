// { dg-do run { target i?86-*-* } }
// { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } }
// { dg-options "-fabi-version=1" }

struct C4
{
   int b:30;
   C4(){};
};

struct C1:  virtual C4
{
  int i;
};

int main() {
  if (sizeof (C1) != 12)
    return 1;
}
