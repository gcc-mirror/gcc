/* PR target/92140 */
/* { dg-do compile { target nonpic } } */
/* { dg-options "-O2 -mtune=generic -masm=att" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$-1, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t\\\$-1, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t\\\$0, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$0, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$25, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t\\\$25, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t\\\$-26, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$-26, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$-43, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t\\\$-43, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t\\\$42, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$42, v" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t%\[a-z0-9]*, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t%\[a-z0-9]*, v" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t\\\$-1, %" 1 } } */

char c;
int v;

__attribute__((noipa)) void f1 (void) { v += c != 0; }
__attribute__((noipa)) void f2 (void) { v -= c != 0; }
__attribute__((noipa)) void f3 (void) { v += c == 0; }
__attribute__((noipa)) void f4 (void) { v -= c == 0; }
__attribute__((noipa)) void f5 (void) { v += (c != 0) - 26; }
__attribute__((noipa)) void f6 (void) { v -= (c != 0) - 26; }
__attribute__((noipa)) void f7 (void) { v += (c == 0) - 26; }
__attribute__((noipa)) void f8 (void) { v -= (c == 0) - 26; }
__attribute__((noipa)) void f9 (void) { v += (c != 0) + 42; }
__attribute__((noipa)) void f10 (void) { v -= (c != 0) + 42; }
__attribute__((noipa)) void f11 (void) { v += (c == 0) + 42; }
__attribute__((noipa)) void f12 (void) { v -= (c == 0) + 42; }
__attribute__((noipa)) void f13 (int z) { v += (c == 0) + z; }
__attribute__((noipa)) void f14 (int z) { v -= (c == 0) + z; }
__attribute__((noipa)) unsigned int f15 (unsigned int n) { return n ? 2 : 1; }
