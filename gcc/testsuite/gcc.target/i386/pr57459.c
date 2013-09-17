/* PR rtl-optimization/57459 */
/* { dg-do run } */
/* { dg-options "-fno-inline -O2 -minline-all-stringops -fno-omit-frame-pointer" } */

int total1[10], total2[10], total3[10], total4[10], total5[10], a[20];
int len;

void stackclean() {
  void *ptr = __builtin_alloca(20000);
  __builtin_memset(ptr, 0, 20000);
}

void foo(const char *s) {
  int r1 = a[1];
  int r2 = a[2];
  int r3 = a[3];
  int r4 = a[4];
  int r5 = a[5];

  len =  __builtin_strlen(s);

  if (s != 0)
    return;

  while (r1) {
   total1[r1] = r1;
   r1--;
  }

  while (r2) {
   total2[r2] = r2;
   r2--;
  }

  while (r3) {
   total3[r3] = r3;
   r3--;
  }

  while (r4) {
   total4[r4] = r4;
   r4--;
  }

  while (r5) {
   total5[r5] = r5;
   r5--;
  }
}

extern void abort (void);

int main() {
  stackclean();
  foo("abcdefgh");
  if (len != 8)
    abort ();
  return 0;
}

