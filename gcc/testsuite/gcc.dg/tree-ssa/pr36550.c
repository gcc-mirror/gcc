/* { dg-do compile } */
/* { dg-options "-Os -Wuninitialized" } */
void bail(void) __attribute__((noreturn));
unsigned once(void);
int pr(char**argv)
{
	char *bug;
	unsigned check = once();
	if (check) {
		if (*argv)
			bug = *++argv;
	} else {
		bug = *argv++;
		if (!*argv)
			bail();
	}
	/* now bug is set except if (check && !*argv) */
	if (check) {
		if (!*argv)
			return 0;
	}
	/* if we ever get here then bug is set */
	return *bug != 'X';
}

