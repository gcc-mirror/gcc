/* Mis-aligned packed structures.  */

void abort (void);
void exit (int);

typedef struct
{
  char b0;
  char b1;
  char b2;
  char b3;
  char b4;
  char b5;
} __attribute__ ((packed)) b_struct;


typedef struct
{
  short a;
  long b;
  short c;
  short d;
  b_struct e;
} __attribute__ ((packed)) a_struct;


int
main(void)
{
  volatile a_struct *a;
  volatile a_struct b;

  a = &b;
  *a = (a_struct){1,2,3,4};
  a->e.b4 = 'c';

  if (a->a != 1 || a->b != 2 || a->c != 3 || a->d != 4 || a->e.b4 != 'c')
    abort ();

  exit (0);
}
