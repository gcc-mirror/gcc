/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */
void func (int x, int y) __attribute__((vector(linear(x:1), uniform (y)),
					vector));

int q;
int main (void)
{
  int ii = 0;
  q = 5; 
  for (ii = 0; ii < 100; ii++) 
    func (ii, q);

  return 0;
}

