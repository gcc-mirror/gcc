/* Test various logical operations.  */

TYPE arg1 (TYPE p, TYPE q) { return p & q; }		/* AND  */
TYPE arg2 (TYPE p, TYPE q) { return p | q; }		/* OR   */
TYPE arg3 (TYPE p, TYPE q) { return p ^ q; }		/* XOR  */
TYPE arg4 (TYPE p)	   { return ~ p; }		/* NOR  */
TYPE arg5 (TYPE p, TYPE q) { return ~(p & q); }		/* NAND */
TYPE arg6 (TYPE p, TYPE q) { return ~(p | q); }		/* NOR  */
TYPE arg7 (TYPE p, TYPE q) { return ~(p ^ q); }		/* EQV  */
TYPE arg8 (TYPE p, TYPE q) { return (~p) & q; }		/* ANDC */
TYPE arg9 (TYPE p, TYPE q) { return (~p) | q; }		/* ORC  */
TYPE arg10(TYPE p, TYPE q) { return (~p) ^ q; }		/* EQV  */
TYPE arg11(TYPE p, TYPE q) { return p & (~q); }		/* ANDC */
TYPE arg12(TYPE p, TYPE q) { return p | (~q); }		/* ORC  */
TYPE arg13(TYPE p, TYPE q) { return p ^ (~q); }		/* EQV  */

void ptr1 (TYPE *p) { p[0] = p[1] & p[2]; }		/* AND  */
void ptr2 (TYPE *p) { p[0] = p[1] | p[2]; }		/* OR   */
void ptr3 (TYPE *p) { p[0] = p[1] ^ p[2]; }		/* XOR  */
void ptr4 (TYPE *p) { p[0] = ~p[1]; }			/* NOR  */
void ptr5 (TYPE *p) { p[0] = ~(p[1] & p[2]); }		/* NAND */
void ptr6 (TYPE *p) { p[0] = ~(p[1] | p[2]); }		/* NOR  */
void ptr7 (TYPE *p) { p[0] = ~(p[1] ^ p[2]); }		/* EQV  */
void ptr8 (TYPE *p) { p[0] = ~(p[1]) & p[2]; }		/* ANDC */
void ptr9 (TYPE *p) { p[0] = (~p[1]) | p[2]; }		/* ORC  */
void ptr10(TYPE *p) { p[0] = (~p[1]) ^ p[2]; }		/* EQV  */
void ptr11(TYPE *p) { p[0] = p[1] & (~p[2]); }		/* ANDC */
void ptr12(TYPE *p) { p[0] = p[1] | (~p[2]); }		/* ORC  */
void ptr13(TYPE *p) { p[0] = p[1] ^ (~p[2]); }		/* EQV  */
