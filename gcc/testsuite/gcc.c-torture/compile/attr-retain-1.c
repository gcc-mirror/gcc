/* { dg-do compile { target R_flag_in_section } } */
/* Prevent readonly data from being put in writable sdata for 32-bit powerpc. */
/* { dg-options "-G0" { target { powerpc*-*-* && ilp32 } } } */
/* { dg-final { scan-assembler ".text.*,\"axR\"" } } */
/* { dg-final { scan-assembler ".bss.*,\"awR\"" } } */
/* { dg-final { scan-assembler ".data.*,\"awR\"" } } */
/* { dg-final { scan-assembler ".rodata.*,\"aR\"" } } */
/* { dg-final { scan-assembler ".data.used_foo_sec,\"awR\"" } } */

void __attribute__((used,retain)) used_fn (void) { }
void unused_fn (void) { }
void __attribute__((hot,used,retain)) used_hot_fn (void) { }
void __attribute__((hot)) unused_hot_fn (void) { }
void __attribute__((cold,used,retain)) used_cold_fn (void) { }
void __attribute__((cold)) unused_cold_fn (void) { }
int __attribute__((used,retain)) used_bss = 0;
int __attribute__((used,retain)) used_data = 1;
const int __attribute__((used,retain)) used_rodata = 2;
int __attribute__((used,retain)) used_comm;
static int __attribute__((used,retain)) used_lcomm;

int unused_bss = 0;
int unused_data = 1;
const int unused_rodata = 2;
int unused_comm;
static int unused_lcomm;

/* Test switching back to the used,retained sections.  */
void __attribute__((used,retain)) used_fn2 (void) { }
int __attribute__((used,retain)) used_bss2 = 0;
int __attribute__((used,retain)) used_data2 = 1;
const int __attribute__((used,retain)) used_rodata2 = 2;
int __attribute__((used,retain)) used_comm2;
static int __attribute__((used,retain)) used_lcomm2;

int __attribute__((used,retain,section(".data.used_foo_sec"))) used_foo = 2;
