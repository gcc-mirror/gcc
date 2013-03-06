/* { dg-lto-do run } */
/* { dg-skip-if "x86 only" { ! { x86_64-*-* i?86-*-* } } { "*" } { "" } } */
/* { dg-skip-if "no .type" { *-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "no @function" { *-*-mingw* *-*-cygwin* } { "*" } { "" } } */

/* Doesn't work without this dummy function with -fwhopr.  */
int foo(void) { }

asm(".text\n"
    ".globl main\n"
    "\t.type main,@function\n"
    "main:\n"
    "\txorl %eax, %eax\n"
    "\tret\n");
