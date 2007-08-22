// Test for bitfield alignment in structs on IA-32
// { dg-do run }
// { dg-options "-O2" }
// { dg-options "-mno-align-double -mno-ms-bitfields" { target *-*-interix* } }

extern void abort (void);
extern void exit (int);

struct X {
  int : 32;
};

struct Y {
  int i : 32;
};

int main () {
  if (__alignof__(struct X) != 1)
    abort ();
  if (__alignof__(struct Y) != 4)
    abort ();
  
  exit (0);
}
