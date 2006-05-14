/* { dg-do run } */
/* { dg-options "-O2" } */

void abort(void);

__attribute__((noinline))
int f (unsigned short word) {  
  return (word & 0x1) && (((unsigned short) (word & 0x8000)) == 0x8000);
}

int main(void) {
  if (!f(0x8001))
    abort();
  return 0;
}

