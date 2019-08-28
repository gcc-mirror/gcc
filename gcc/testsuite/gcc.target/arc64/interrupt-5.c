/* { dg-options "-O" } */
extern void will_trig_exception(void);

__attribute__ ((interrupt))
void isr_0 (void)
{
  will_trig_exception();
}

/* 0. There shouldn't be any need to (re)adjust the stack pointer.  */
/* { dg-final { scan-assembler-not "\\sadd.*sp" } } */
/* { dg-final { scan-assembler-not "\\ssub.*sp" } } */

/* 1. hs6x output without double loads and stores.  */
/* { dg-final { scan-assembler "pushl_s\\s+r58\n"   { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r30\n"   { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r13\n"   { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r12\n"   { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r11\n"   { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r10\n"   { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r9\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r8\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r7\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r6\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r5\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r4\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r3\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r2\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r1\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r0\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+blink\n" { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+blink\n"  { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r0\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r1\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r2\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r3\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r4\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r5\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r6\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r7\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r8\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r9\n"     { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r10\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r11\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r12\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r13\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r30\n"    { target { hs6x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r58\n"    { target { hs6x && { ! doubleaccess } } } } } */

/* 2. hs6x output with double loads and stores.  */
/* { dg-final { scan-assembler "pushl_s\\s+r58\n"   { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+r30\n"   { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r12,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r10,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r8,"      { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r6,"      { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r4,"      { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r2,"      { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "stdl.a\\s+r0,"      { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "pushl_s\\s+blink\n" { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "popl_s\\s+blink\n"  { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r0,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r2,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r4,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r6,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r8,"     { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r10,"    { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "lddl.ab\\s+r12,"    { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r30\n"    { target { hs6x && doubleaccess } } } } */
/* { dg-final { scan-assembler "popl_s\\s+r58\n"    { target { hs6x && doubleaccess } } } } */

/* 3. hs5x output without double loads and stores.  */
/* { dg-final { scan-assembler "st.a\\s+r58,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r30,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r13,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r12,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r11,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r10,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r9,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r8,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r7,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r6,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r5,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r4,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r3,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r2,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r1,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+r0,"     { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "st.a\\s+blink,"  { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+blink," { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r0,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r1,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r2,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r3,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r4,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r5,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r6,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r7,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r8,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r9,"    { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r10,"   { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r11,"   { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r12,"   { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r13,"   { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r30,"   { target { hs5x && { ! doubleaccess } } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r58,"   { target { hs5x && { ! doubleaccess } } } } } */

/* 4. hs5x output with double loads and stores.  */
/* { dg-final { scan-assembler "st.a\\s+r58,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "st.a\\s+r30,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r12,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r10,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r8,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r6,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r4,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r2,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "std.a\\s+r0,"    { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "st.a\\s+blink,"  { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+blink," { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r0,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r2,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r4,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r6,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r8,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r10,"  { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ldd.ab\\s+r12,"  { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r30,"   { target { hs5x && doubleaccess } } } } */
/* { dg-final { scan-assembler "ld.ab\\s+r58,"   { target { hs5x && doubleaccess } } } } */
