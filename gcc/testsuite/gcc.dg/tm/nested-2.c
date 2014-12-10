/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */
/* { dg-add-options bind_pic_locally } */

void foobar(void)
{
    __transaction_atomic {
       foobar();
    }
}

void doit(void) __attribute__((transaction_safe));

__attribute__((transaction_callable))
void callable(void)
{
  __transaction_atomic {
    doit();
  }
}
