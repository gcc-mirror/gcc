/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-require-effective-target trampolines } */

static void pp(void)
{
	int pp;
	{
		auto void pp(void);
		void pp(void) { }
	}
}

static void q2(void);

static void qq(void)
{
	auto void q2(void);
	void q2(void) { }
}

