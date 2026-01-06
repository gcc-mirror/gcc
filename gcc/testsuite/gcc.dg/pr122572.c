/* { dg-do run } */
/* { dg-options "-std=c23 -O2" } */

typedef struct {
	int *arg;
	int flags;
} optStruct;

typedef struct {
	int *specified;
	int flags;
} optEntry;

typedef struct {
	int short_allowed;
	optStruct *opt_table;
} optStruct2;

typedef struct {
	int short_allowed;
	optEntry *opt_table;
} optStruct3;

[[gnu::noipa]]
void f(int, int, optStruct3 a)
{
	 a.opt_table[0].flags = 1;
}

[[gnu::noipa]]
int alias_bug (void)
{
	static optEntry option_def[50];
	static optStruct3 opt;
	option_def[0].flags = 0;
	opt.opt_table = option_def;
	f (0, 0, opt);
	return opt.opt_table[0].flags;
}

int main()
{
	if (1 != alias_bug())
		__builtin_abort();
}

