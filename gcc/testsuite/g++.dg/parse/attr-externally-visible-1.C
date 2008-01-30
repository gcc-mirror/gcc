// { dg-do compile }
// { dg-options "-O3 -fwhole-program" }
// { dg-options "-O3 -fwhole-program -fpie" { target { ! nonpic } } }
// { dg-final { scan-assembler "foo1" } }
// { dg-final { scan-assembler "foo2" } }
// { dg-final { scan-assembler "foo3" } }
// { dg-final { scan-assembler "foo4" } }
// { dg-final { scan-assembler "foo5" } }
// { dg-final { scan-assembler-not "foo6" } }
// { dg-final { scan-assembler "bar1" } }
// { dg-final { scan-assembler "bar2" } }
// { dg-final { scan-assembler "bar3" } }
// { dg-final { scan-assembler "bar4" } }
// { dg-final { scan-assembler "bar5" } }
// { dg-final { scan-assembler-not "bar6" } }

extern void foo1 (void) __attribute__((externally_visible));
void foo1 (void) { }

extern void foo2 (void) __attribute__((externally_visible));
__attribute__((externally_visible)) void foo2 (void) { }

extern void foo3 (void);
__attribute__((externally_visible)) void foo3 (void) { }

__attribute__((externally_visible)) void foo4 (void) { }

void foo5 (void) { }
extern void foo5 (void) __attribute__((externally_visible));

void foo6 (void) { }

extern char *bar1 __attribute__((externally_visible));
char *bar1;

extern char *bar2 __attribute__((externally_visible));
char *bar2 __attribute__((externally_visible));

extern char *bar3;
char *bar3 __attribute__((externally_visible));

char *bar4 __attribute__((externally_visible));

char *bar5;
extern char *bar5 __attribute__((externally_visible));

char *bar6;

int main (void) { }
