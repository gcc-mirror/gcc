/* { dg-do compile } */
/* { dg-options "-std=c23 -Wzero-as-null-pointer-constant" } */

#define NULL (void*)0

void foo(void*);
void baz(bool);

void bar()
{
	foo(0);				/* { dg-warning "zero as null pointer constant" } */
	foo(NULL);
	foo(nullptr);

	int a = 0;

	bool b = 0;
	bool c = (void*)0;
	bool d = NULL;
	bool e = nullptr;

	bool f = (bool)0;
	bool g = (bool)(void*)0;
	bool h = (bool)NULL;
	bool i = (bool)nullptr;

	baz(0);
	baz((void*)0);
	baz(NULL);
	baz(nullptr);
	baz((bool)0);
	baz((bool)(void*)0);
	baz((bool)NULL);
	baz((bool)nullptr);

	void *p = 0;			/* { dg-warning "zero as null pointer constant" } */
	void *r = NULL;
	void *t = nullptr;
	void *o = { };
	void *q = { 0 };		/* { dg-warning "zero as null pointer constant" } */
	void *s = { NULL };
	void *u = { nullptr };
	struct { void *q; } x = { 0 };	/* { dg-warning "zero as null pointer constant" } */
	struct { void *q; } y = { NULL };
	struct { void *q; } z = { nullptr };
	struct { int a; void *b; } n = { 0 };
	struct { int a; int b; void *c; } m = { 0, 0 };

	1 ? 0 : 0;
	1 ? i : 0;
	1 ? 0 : p;			/* { dg-warning "zero as null pointer constant" } */
	1 ? p : 0;			/* { dg-warning "zero as null pointer constant" } */
	1 ? 0 : NULL;			/* { dg-warning "zero as null pointer constant" } */
	1 ? NULL : 0;			/* { dg-warning "zero as null pointer constant" } */

	0 ? 0 : 1;
	(void*)0 ? 0 : 1;
	NULL ? 0 : 1;
	nullptr ? 0 : 1;

	if (0 == 0);
	if (i == 0);
	if (p == 0);			/* { dg-warning "zero as null pointer constant" } */
	if (0 == p);			/* { dg-warning "zero as null pointer constant" } */
	if (NULL == 0);			/* { dg-warning "zero as null pointer constant" } */
	if (0 == NULL);			/* { dg-warning "zero as null pointer constant" } */
	if (0 == (void*)0);		/* { dg-warning "zero as null pointer constant" } */
	if (0 == nullptr);		/* { dg-warning "zero as null pointer constant" } */
	if (nullptr == 0);		/* { dg-warning "zero as null pointer constant" } */

	if (0 != 0);
	if (i != 0);
	if (p != 0);			/* { dg-warning "zero as null pointer constant" } */
	if (0 != p);			/* { dg-warning "zero as null pointer constant" } */
	if (NULL != 0);			/* { dg-warning "zero as null pointer constant" } */
	if (0 != NULL);			/* { dg-warning "zero as null pointer constant" } */
	if (0 != (void*)0);		/* { dg-warning "zero as null pointer constant" } */
	if (0 != nullptr);		/* { dg-warning "zero as null pointer constant" } */
	if (nullptr != 0);		/* { dg-warning "zero as null pointer constant" } */

	if (0);
	if (NULL);
	if ((void*)0);
	if (nullptr);
	if (!0);
	if (!NULL);
	if (!(void*)0);
	if (!nullptr);

	if (p);
	p ? 0 : 1;
	if (!p);
	(!p) ? 0 : 1;

	int *v;
	if (v);
	v ? 1 : 0;
	if (!v);
	(!v) ? 1 : 0;

	bool j = p;
	bool k = (bool)p;

	baz(p);
	baz((bool)p);
}

