/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -g -fomit-frame-pointer -fdump-rtl-vartrack-details-slim" } */

static volatile int vv = 1;

extern long foo (long x);

int
main ()
{
  long x = vv;
  foo (x);
  foo (x + 1);
  return 0;
}

/* Before adjust_insn:
   26: [--sp:DI]=b[px]:DI
   29: b[px]:DI=[sp:DI++]

   after adjust_insn:
   26: {[argp:DI-0x10]=b[px]:DI;sp:DI=argp:DI-0x10;}
   29: {b[px]:DI=[argp:DI-0x10];sp:DI=argp:DI-0x8;} */

/* { dg-final { scan-rtl-dump-times {[0-9][0-9]*: \{\[argp:DI-0x10\]=b[px]:DI;sp:DI=argp:DI-0x10;\}} 1 "vartrack" } } */

/* { dg-final { scan-rtl-dump-times {[0-9][0-9]*: \{b[px]:DI=\[argp:DI-0x10\];sp:DI=argp:DI-0x8;\}} 1 "vartrack" } } */

