/* Currently the warning only fires at -O0
   (needs to inline the call without optimizing the
   implicit default of the switch).  */

/* { dg-additional-options "-O0" } */

typedef __SIZE_TYPE__ size_t;
int snprintf(char *str, size_t size, const char *format, ...);

enum usage_kind {
	USAGE_ERROR,
	USAGE_BUG,
};

static void __analyzer_vreportf(enum usage_kind kind)
{
	char buf[256];
	const char *pfx;

	switch (kind) { /* { dg-message "following 'default:' branch" } */
	case USAGE_ERROR:
		pfx = "error: ";
		break;
	case USAGE_BUG:
		pfx = "BUG: ";
		break;
	}

	if (kind == USAGE_BUG)
		snprintf(buf, sizeof(buf), "%s%s:%d: ", pfx, "file", 123);
	else
		snprintf(buf, sizeof(buf), "%s", pfx); /* { dg-warning "uninitialized" } */
}

int main(void)
{
	__analyzer_vreportf(42);

	return 0;
}
