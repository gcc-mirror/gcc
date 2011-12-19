// { dg-do compile }
// { dg-options "-fgnu-tm -O" }

struct A
{
  __attribute__ ((transaction_safe)) virtual void virtfoo () { }
};
