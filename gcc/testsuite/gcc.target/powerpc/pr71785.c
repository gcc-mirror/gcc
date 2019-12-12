/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */
/* We have to lose the default pic codegen on Darwin.  */
/* { dg-additional-options "-mdynamic-no-pic" { target powerpc*-*-darwin* } } */
/* ... and account for the out-of-line GPR restore.  */
/* { dg-final { scan-assembler-times {\mb[ \t]*restGPR} 1 { target powerpc*-*-darwin* } } } */
/* { dg-final { scan-assembler-not {\mb[ \t]L} { target powerpc*-*-darwin* } } } */
/* { dg-final { scan-assembler-not {\mb\M} { target { ! powerpc*-*-darwin* } } } } */

/* Check that all computed gotos in this testcase end up unfactored completely.
   If some is not there will be a unconditional jump left; if all works fine,
   all are gone.  */

typedef enum opcode
{
	OP_A,
	OP_B,
	OP_END
} opcode;

typedef struct op
{
	opcode opcode;
	int arg;
} op;

extern void do_stuff_b(int arg);
extern void do_stuff_c(int arg);

extern int someglobal;

void
eval(op *op)
{
	static const void *dispatch_table[] = {
		&&CASE_OP_A,
		&&CASE_OP_B,
		&&CASE_OP_C,
		&&CASE_OP_END
	};

	goto *dispatch_table[op->opcode];
CASE_OP_A:
	someglobal++;
	op++;
	goto *dispatch_table[op->opcode];
CASE_OP_B:
	do_stuff_b(op->arg);
	op++;
	goto *dispatch_table[op->opcode];
CASE_OP_C:
	do_stuff_c(op->arg);
	op++;
	goto *dispatch_table[op->opcode];
CASE_OP_END:
	return;
}
