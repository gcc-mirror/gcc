/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O0" } */

typedef struct {
	int value[5];
} type_t;

__attribute__((transaction_safe))
type_t func_move ();

__attribute__((transaction_safe))
type_t func_push (int type)
{
	type_t trace;

	if (type == 9)
		trace = func_move();

	return trace;
}
