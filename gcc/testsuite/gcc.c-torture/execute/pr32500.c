extern void abort(void);
extern void exit(int);
void foo(int) __attribute__((noinline));
void bar(void) __attribute__((noinline));

/* Make sure foo is not inlined or considered pure/const.  */
int x;
void foo(int i) { x = i; }
void bar(void) { exit(0); }

int
main(int argc, char *argv[])
{
	int i;
	int numbers[4] = { 0xdead, 0xbeef, 0x1337, 0x4242 };

	for (i = 1; i <= 12; i++) {
		if (i <= 4)
			foo(numbers[i]);
		else if (i >= 7 && i <= 9)
			bar();
	}

	abort();
}

