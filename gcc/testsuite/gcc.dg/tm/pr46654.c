/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

extern void baz(int);

int y;
void foo(volatile int x)
{
  __transaction_atomic {
    x = 5; /* { dg-error "invalid use of volatile lvalue inside transaction" } */
    x += y;
    y++;
  }
  baz(x);
}


volatile int i = 0;

void george()
{
  __transaction_atomic {
   if (i == 2) /* { dg-error "invalid use of volatile lvalue inside transaction" } */
     i = 1;
  }
}
