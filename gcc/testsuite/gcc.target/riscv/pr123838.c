/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og"} } */

#define F(NAME, OP, TYPE) TYPE f##NAME##TYPE (TYPE x, TYPE n) { return x OP (sizeof (TYPE) * 8 - 1 - n); }

F(RSHIFT, >>, int)
F(LSHIFT, <<, int)
F(RSHIFT, >>, long)
F(LSHIFT, <<, long)

/* { dg-final { scan-assembler-times "not\t" 4 } } */
/* { dg-final { scan-assembler-not "li\t" } } */
/* { dg-final { scan-assembler-not "sub\t" } } */
/* { dg-final { scan-assembler-not "subw\t" } } */
/* { dg-final { scan-assembler-not "andi\t" } } */

