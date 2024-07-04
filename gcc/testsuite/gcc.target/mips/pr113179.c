/* Check if the operand of INS is sign-extended on MIPS64.  */
/* { dg-options "-mips64r2 -mabi=64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

struct xx {
        int a:1;
        int b:24;
        int c:6;
        int d:1;
};

long long xx (struct xx *a, long long b) {
        a->d = b;
        return b+1;
}

/* { dg-final { scan-assembler "\tsll\t\\\$3,\\\$5,0" } } */
/* { dg-final { scan-assembler "\tdaddiu\t\\\$2,\\\$5,1" } } */
