/* { dg-do run { target { { ! x32 } && avx512f_runtime } } } */
/* { dg-do compile { target { { ! x32 } && { ! avx512f_runtime } } } } */
/* { dg-options "-Ofast -mabi=ms -mavx512f" } */
/* { dg-require-effective-target avx512f } */

/* Test when calling a sysv func.  */

int a[56];
int b;

static void __attribute__((sysv_abi)) sysv ()
{
}

void __attribute__((sysv_abi)) (*volatile const sysv_noinfo)() = sysv;

int main (int argc, char *argv[]) {
  int c;
  sysv_noinfo ();
  for (; b; b++) {
    c = b;
    if (b & 1)
      c = 2;
    a[b] = c;
  }
  return 0;
}
