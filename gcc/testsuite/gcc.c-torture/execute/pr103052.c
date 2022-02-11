static void js_error(void);
static int top;
static void js_throw(void)
{
	__builtin_exit(0);
}

// LOCATION A -- if js_pop is here, the bug is present
static void js_pop(void)
{
	if (++top > 100)
		js_error();
}

static void jsC_error(const char *v)
{
	if (v[0] == 0)
		js_error();
	js_throw();
}
static void checkfutureword(const char *exp)
{
	if (!__builtin_strcmp(exp, "const"))
		jsC_error("boom");
}
static void js_error(void) {
	checkfutureword("foo");
	checkfutureword("bar");
	js_pop();
}
int main(void)
{
	checkfutureword("const");
	__builtin_abort ();
}
