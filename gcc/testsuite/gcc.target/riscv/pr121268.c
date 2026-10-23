/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcb_zicond -mabi=lp64d" { target rv64 } } */
/* { dg-additional-options "-march=rv32gcb_zicond -mabi=ilp32" { target rv32 } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

#define F1(TYPE, NAME) TYPE f1##NAME (TYPE x) { return ~(x | -x); }
#define F2(TYPE, NAME) TYPE f2##NAME (TYPE x) { return ~x & (x - 1); }
#define F3(TYPE, NAME) TYPE f3##NAME (TYPE x) { return (x & -x) - 1; }

F1 (unsigned char, uc)
F1 (unsigned short, us)
F1 (unsigned int, ui)
F1 (unsigned long, ul)
F1 (signed char, sc)
F1 (signed short, ss)
F1 (signed int, si)
F1 (signed long, sl)

F2 (unsigned char, uc)
F2 (unsigned short, us)
F2 (unsigned int, ui)
F2 (unsigned long, ul)
F2 (signed char, sc)
F2 (signed short, ss)
F2 (signed int, si)
F2 (signed long, sl)

F3 (unsigned char, uc)
F3 (unsigned short, us)
F3 (unsigned int, ui)
F3 (unsigned long, ul)
F3 (signed char, sc)
F3 (signed short, ss)
F3 (signed int, si)
F3 (signed long, sl)

/* We match addi and addiw here on purpose.  */
/* { dg-final { scan-assembler-times "addi" 24 } } */

/* The transformation we want to apply for one class of test requires
   nonzero bits information to be safe on rv64.  That information is
   not available for the long of that test. Hence the difference
   in andn instrutions seen across rv32 and rv64.  */
/* { dg-final { scan-assembler-times "andn\t" 24 { target rv32 } } } */
/* { dg-final { scan-assembler-times "andn\t" 22 { target rv64 } } } */
