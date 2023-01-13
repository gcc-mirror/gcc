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

	switch (kind) {
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
		snprintf(buf, sizeof(buf), "%s", pfx);
}

int main(void)
{
	__analyzer_vreportf(USAGE_ERROR);
	__analyzer_vreportf(USAGE_BUG);

	return 0;
}
