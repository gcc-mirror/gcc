/* { dg-options "-O2" }  */

struct A {
  unsigned int f1 : 3;
  unsigned int f2 : 3;
  unsigned int f3 : 1;
  unsigned int f4 : 1;

};

void init_A (struct A *this)
{
  this->f1 = 0;
  this->f2 = 1;
  this->f3 = 0;
  this->f4 = 0;
}

/* { dg-final { scan-assembler-times "ldr" 1 } } */
