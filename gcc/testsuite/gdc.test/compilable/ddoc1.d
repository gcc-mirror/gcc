// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 1
// REQUIRED_ARGS: -d

/** This module is for ABC
 * Copyright: Copyright &copy;
 */

module abc;

string foos = "foo";

alias int myint; ///
alias int mytypedefint;

/** windy
 * city
 *
 * paragraph 2 about of F $$(NAME)
 * -----
 * #include <stdio.h>
  * void main()
 * {
 * 	printf("hello\n");
 * }
 * -----
 * Copyright: 1998
 */
myint f;
enum E { e } /// comment1
int g; /// comment2
private int h; /// comment for H
static int i;
int j;
wchar LS = 0x2028;	/// UTF line separator
wchar PS = 0x2029;	/// UTF paragraph separator

wchar _XX;	/// ditto
wchar YY;	/// ditto

/** Function foo takes argument c and adds it to argulid.
 *
 * Then it munges argulid, u <u>underline</u>. <!-- c, argulid, b -->
 * Params:
 *	c = the character which adds c to argulid
 *	argulid = the argument
 *	u = the other argument
 */
int foo(char c, int argulid, char u);

int barr() { return 3; } /// doc for barr()

/++ The Class Bar +/
class Bar
{
    int x;	/// member X
    int y;	/// member Y
    protected int z;	/// member Z
}

/++ The Enum Easy +/

enum Easy : int
{
	red,	/// the Red
	blue,	/// the Blue
	green,	/// the Green
}
