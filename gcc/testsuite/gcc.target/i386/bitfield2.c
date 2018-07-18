// Test for bitfield alignment in structs on IA-32
// { dg-do run }
// { dg-require-effective-target ia32 }
// { dg-options "-O2 -mno-align-double -mno-ms-bitfields" }

extern void abort (void);
extern void exit (int);

struct X {
  char a;
  long long : 0;
  char b;
} x;

int main () {
  if (&x.b - &x.a != 4)
    abort ();
  if (sizeof (x) != 5)
    abort ();
  
  exit (0);
}
