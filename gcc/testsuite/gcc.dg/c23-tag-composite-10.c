/* { dg-do run } */
/* { dg-options "-std=c23" } */

// test padding works correctly

static struct fo { 
	int a :1; 
	long  :3; 
	int b: 1;
} x = { };

static void foo(void* p)
{
	struct fo { 
		int a :1; 
		long  :3; 
		int b: 1;
	} y;

	typeof(*(1 ? &x : &y))* z = p;
	__builtin_clear_padding(z);
}

int main()
{
	struct fo *p = __builtin_malloc(sizeof *p);
	__builtin_memset(p, 0xFFFF, sizeof *p);
	foo(p);
	p->a = 0;
	p->b = 0;
	if (0 != __builtin_memcmp(p, &x, sizeof *p))
		__builtin_abort();
	return 0;
}

