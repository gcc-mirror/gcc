/* Test volatile access to unaligned field.  */
/* { dg-do compile } */
/* { dg-options "-fno-strict-volatile-bitfields -fdump-rtl-final" } */

#define test_type unsigned short

typedef struct s{
 unsigned char Prefix[1];
 volatile test_type Type;
}__attribute((__packed__,__aligned__(4))) ss;

extern volatile ss v;

void
foo (test_type u)
{
  v.Type = u;
}

/* The C++ memory model forbids data store race conditions outside the
   unaligned data member, therefore only QI or HI access is allowed, no SI.  */
/* { dg-final { scan-rtl-dump-not "mem/v(/.)*:SI" "final" } } */
