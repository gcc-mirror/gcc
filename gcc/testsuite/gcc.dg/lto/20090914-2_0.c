/* { dg-lto-do run { target x86_64-*-* i?86-*-* } } */

/* Doesn't work without this dummy function with -fwhopr.  */
int foo(void) { }

asm(".text\n"
    ".globl main\n"
    "\t.type main,@function\n"
    "main:\n"
    "\txorl %eax, %eax\n"
    "\tret\n");
