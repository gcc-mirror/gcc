/* { dg-do compile } */
/* { dg-options "" } */

extern int a(void);	// external linkage (6.2.2p4)
static int a(void);	/* { dg-error "static declaration of 'a' follows non-static declaration" } */

static int b(void);	// internal linkage (6.2.2p3)
extern int b(void);	// internal linkage (6.2.2p4)

static int h0(void);

void s(void)
{
	extern int h0(void);	// internal linkage (6.2.2p4),
	extern int h0(void); 	// internal linkage (6.2.2p4), redeclaration, ok
	extern int h2(void);	// external linkage (6.2.2p4)
	extern int h2(void);	// external linkage (6.2.2p4), redeclaration, ok.
}


extern int i(void);	// external linkage (6.2.2p4)
static int j(void);	// internal linkage (6.2.2p3)

void bar(void)
{
	extern int i(void);	// external linkage (6.2.2p4), ok
}

void foo(void)
{
	extern int j(void);	// internal linkage (6.2.2p4), ok, internal
}

void x(void)
{
	int i(void);		// no linkage (6.2.2p6)
	int j;			// no linkage (6.2.2p6)
	{
		extern int j(void);	/* { dg-error "function previously declared 'static' redeclared 'extern'" } */
	}
}

