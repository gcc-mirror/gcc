// PERMUTE_ARGS:
// REQUIRED_ARGS: -verrors=simple -D -Dd${RESULTS_DIR}/compilable -wi -o-

/*
TEST_OUTPUT:
---
compilable/ddoc10236.d(35): Warning: Ddoc: parameter count mismatch, expected 2, got 1
compilable/ddoc10236.d(47): Warning: Ddoc: function declaration has no parameter 'y'
compilable/ddoc10236.d(59): Warning: Ddoc: function declaration has no parameter 'y'
compilable/ddoc10236.d(59): Warning: Ddoc: parameter count mismatch, expected 1, got 2
compilable/ddoc10236.d(71): Warning: Ddoc: parameter count mismatch, expected 2, got 0
compilable/ddoc10236.d(71):        Note that the format is `param = description`
---
*/

/***********************************
 * foo_good does this.
 * Params:
 *	x =	is for this
 *		and not for that
 *	y =	is for that
 */

void foo_good(int x, int y)
{
}

/***********************************
 * foo_count_mismatch does this.
 * Params:
 *	x =	is for this
 *		and not for that
 */

void foo_count_mismatch(int x, int y)	// Warning: Ddoc: parameter count mismatch
{
}

/***********************************
 * foo_no_param_y does this.
 * Params:
 *	x =	is for this
 *		and not for that
 *	y =	is for that
 */

void foo_no_param_y(int x, int z)		// Warning: Ddoc: function declaration has no parameter 'y'
{
}

/***********************************
 * foo_count_mismatch_no_param_y does this.
 * Params:
 *	x =	is for this
 *		and not for that
 *	y =	is for that
 */

void foo_count_mismatch_no_param_y(int x)
{
}

/***********************************
 * foo_count_mismatch_wrong_format does this.
 * Params:
 *	x :	is for this
 *		and not for that
 *	y :	is for that
 */

void foo_count_mismatch_wrong_format(int x, int y)
{
}
