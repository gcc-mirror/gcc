/* Tests for syntax checking of #pragma pack.
   Contributed by Mike Coleman <mcoleman2@kc.rr.com> */

/* { dg-do compile { target *-*-linux* *-*-cygwin* powerpc*-*-eabi* } } */

#pragma pack(pop)               /* { dg-warning "without matching" } */

#pragma pack(push)
#pragma pack(pop)               /* reset */

#pragma pack(push, foo, 1)
#pragma pack(pop, foo, 1)       /* { dg-warning "malformed" } (/
#pragma pack(pop)               /* reset */

#pragma pack(push, foo, 1)
#pragma pack(pop, bar)          /* { dg-warning "without matching" } */
#pragma pack(pop)               /* reset */

#pragma pack(push, foo, 1)
#pragma pack(pop)
#pragma pack(pop, foo)          /* { dg-warning "without matching" } */

#pragma pack(push, foo, 3)      /* { dg-warning "small power of two" } */

extern int blah;	/* prevent "ISO C forbids an empty source file" */
