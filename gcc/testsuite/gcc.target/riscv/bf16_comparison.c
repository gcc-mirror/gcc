/* { dg-do compile } */
/* { dg-options "-march=rv32i -mabi=ilp32 -O" { target { rv32 } } } */
/* { dg-options "-march=rv64i -mabi=lp64 -O" { target { rv64 } } } */

/* 1) bf -> sf               (call	__extendbfsf2)  */
/* 2) sf1 [<|<=|>|>=|==] sf2 (call	__[lt|le|gt|ge|eq]sf2)  */
extern __bf16 bf;
extern __bf16 bf1;
extern __bf16 bf2;

void bf_lt_bf () { bf = (bf1 < bf2) ? bf1 : bf2; }

void bf_le_bf () { bf = (bf1 <= bf2) ? bf1 : bf2; }

void bf_gt_bf () { bf = (bf1 > bf2) ? bf1 : bf2; }

void bf_ge_bf () { bf = (bf1 >= bf2) ? bf1 : bf2; }

void bf_eq_bf () { bf = (bf1 == bf2) ? bf1 : bf2; }

void bf_lt_const () { bf = (bf1 < 3.14f) ? bf1 : bf2; }

void bf_le_const () { bf = (bf1 <= 3.14f) ? bf1 : bf2; }

void const_gt_bf () { bf = (3.14f > bf2) ? bf1 : bf2; }

void const_ge_bf () { bf = (3.14f >= bf2) ? bf1 : bf2; }

void bf_eq_const () { bf = (bf1 == 3.14f) ? bf1 : bf2; }

/* { dg-final { scan-assembler-times "call\t__extendbfsf2" 15 } } */
/* { dg-final { scan-assembler-not "call\t__ltbf2" } } */
/* { dg-final { scan-assembler-not "call\t__lebf2" } } */
/* { dg-final { scan-assembler-not "call\t__gtbf2" } } */
/* { dg-final { scan-assembler-not "call\t__gebf2" } } */
/* { dg-final { scan-assembler-not "call\t__eqbf2" } } */
