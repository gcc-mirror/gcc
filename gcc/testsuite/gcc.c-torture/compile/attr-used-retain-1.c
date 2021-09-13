/* { dg-do compile } */
/* { dg-require-effective-target R_flag_in_section } */
/* { dg-final { scan-assembler-not ".text.*,\"axR\"" } } */
/* { dg-final { scan-assembler-not ".bss.*,\"awR\"" } } */
/* { dg-final { scan-assembler-not ".data.*,\"awR\"" } } */
/* { dg-final { scan-assembler-not ".rodata.*,\"aR\"" } } */
/* { dg-final { scan-assembler-not ".data.used_foo_sec,\"awR\"" } } */

void __attribute__((used)) used_fn (void) { }
void unused_fn (void) { }
void __attribute__((hot,used)) used_hot_fn (void) { }
void __attribute__((hot)) unused_hot_fn (void) { }
void __attribute__((cold,used)) used_cold_fn (void) { }
void __attribute__((cold)) unused_cold_fn (void) { }
int __attribute__((used)) used_bss = 0;
int __attribute__((used)) used_data = 1;
const int __attribute__((used)) used_rodata = 2;
int __attribute__((used)) used_comm;
static int __attribute__((used)) used_lcomm;

int unused_bss = 0;
int unused_data = 1;
const int unused_rodata = 2;
int unused_comm;
static int unused_lcomm;

/* Test switching back to the retained sections.  */
void __attribute__((used)) used_fn2 (void) { }
int __attribute__((used)) used_bss2 = 0;
int __attribute__((used)) used_data2 = 1;
const int __attribute__((used)) used_rodata2 = 2;
int __attribute__((used)) used_comm2;
static int __attribute__((used)) used_lcomm2;

int __attribute__((used,section(".data.used_foo_sec"))) used_foo = 2;
