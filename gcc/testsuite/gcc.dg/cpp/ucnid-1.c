/* { dg-do run } */
/* { dg-options "-std=c99 -fextended-identifiers" } */
void abort (void);

#define \u00C0 1
#define \u00C1 2
#define \U000000C2 3
#define wh\u00ff 4
#define a\u00c4b\u0441\U000003b4e 5

int main (void)
{
  
  if (\u00C0 != 1)
    abort ();
  if (\u00c1 != 2)
    abort ();
  if (\u00C2 != 3)
    abort ();
  if (wh\u00ff != 4)
    abort ();
  if (a\u00c4b\u0441\U000003b4e != 5)
    abort ();
  
  return 0;
}
