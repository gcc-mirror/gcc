/* { dg-do run }
 * { dg-options "-std=c99" }
 * */


// For the conditional operator and variably modified types,
// verify that the size expression on the selected branch
// is evaluated and the correct result is returned.


// keep track which side was evaluated.
static int fc[2] = { 0 };

static int f(int s, int c)
{
	fc[c]++;
	return s;
}


int main()
{
	// two VLAs, constant condition

	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(1 ? (char(*)[ f(5, 0) ])0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(0 ? (char(*)[ f(5, 0) ])0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();	// fails

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();	// fails

	// two VLAs 

	int c = 0;
	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();	// fails

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();

	c = 1;
	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// VLA + array of unknown size, VLA side is evaluated, defined

	c = 0;
	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(c ? (char(*)[ ])0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();

	c = 1;
	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (char(*)[ ])0)))
		__builtin_abort();

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// VLA + array of unknown size, VLA side is not evaluated

	c = 1;
	fc[0] = fc[1] = 0;

	sizeof(*(c ? (char(*)[ ])0 : (char(*)[ f(3, 1) ])0));

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// without sizeof

	fc[0] = fc[1] = 0;

	(c ? (char(*)[ ])0 : (char(*)[ f(3, 1) ])0);

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	c = 0;
	fc[0] = fc[1] = 0;

	sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (char(*)[ ])0));

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// without sizeof

	fc[0] = fc[1] = 0;

	(c ? (char(*)[ f(5, 0) ])0 : (char(*)[ ])0);

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();


	// VLA + array of known size, VLA side is evaluated

	c = 0;
	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(c ? (char(*)[3])0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	// sizeof is not evaluated because not a VLA
	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	c = 0;
	fc[0] = fc[1] = 0;

	// without sizeof

	(c ? (char(*)[3])0 : (char(*)[ f(3, 1) ])0);

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();

	c = 1;
	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (char(*)[5])0)))
		__builtin_abort();

	// sizeof is not evaluated because not a VLA
	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// without sizeof

	fc[0] = fc[1] = 0;

	(c ? (char(*)[ f(5, 0) ])0 : (char(*)[ 5 ])0);

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// VLA + array of known size, VLA side is not evaluated

	c = 0;
	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(c ? (char(*)[ f(3, 0) ])0 : (char(*)[ 3 ])0)))
		__builtin_abort();

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	c = 1;
	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(c ? (char(*)[ 5 ])0 : (char(*)[ f(5, 1) ])0)))
		__builtin_abort();

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// VM type on one side, null pointer on the other side

	c = 0;
	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(c ? (void*)0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();

	c = 1;
	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (void*)0)))
		__builtin_abort();

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

#if 0
	// these cases are not fixable
	// VM types on one side, null pointer on the other side
	c = 1;
	fc[0] = fc[1] = 0;

	if (3 != sizeof(*(c ? (void*)0 : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();

	c = 0;
	fc[0] = fc[1] = 0;

	if (5 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : (void*)0)))
		__builtin_abort();

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();
#endif

	// VLA + void*
	void* p = 0;
	c = 0;
	fc[0] = fc[1] = 0;

	if (1 != sizeof(*(c ? p : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	// not a VLA or evaluated
	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// .. without sizeof

	(c ? p : (char(*)[ f(3, 1) ])0);

	if ((0 != fc[0]) || (1 != fc[1]))
		__builtin_abort();

	c = 1;
	fc[0] = fc[1] = 0;

	if (1 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : p)))
		__builtin_abort();

	// not a VLA
	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// .. without sizeof

	(c ? (char(*)[ f(5, 0) ])0 : p);

	if ((1 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// VLA + void*, VLA side not evaluated

	c = 1;
	fc[0] = fc[1] = 0;

	if (1 != sizeof(*(c ? p : (char(*)[ f(3, 1) ])0)))
		__builtin_abort();

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// .. without sizeof

	(c ? p : (char(*)[ f(3, 1) ])0);

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	c = 0;
	fc[0] = fc[1] = 0;

	if (1 != sizeof(*(c ? (char(*)[ f(5, 0) ])0 : p)))
		__builtin_abort();

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	// .. without sizeof

	(c ? (char(*)[ f(5, 0) ])0 : p);

	if ((0 != fc[0]) || (0 != fc[1]))
		__builtin_abort();

	return 0;
}

