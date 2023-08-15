/* { dg-do run }
 * { dg-options "-std=c23 -fpermissive" }
 */

// nesting and parameters

#define product_type(T, A, B) \
struct product_ ## T { A a ; B b ; }
#define sum_type(T, A, B) \
struct sum_ ## T { _Bool flag ; union { A a ; B b ; }; }

float foo1(product_type(iSfd_, int, sum_type(fd, float, double)) x)
{
	return x.b.a;
}

static void test1(void)
{
	product_type(iSfd_, int, sum_type(fd, float, double)) y = { 3, { 1, { .a = 1. } } };
	product_type(iSfd_, int, sum_type(fd, float, double)) z = y;
	product_type(iSfd_, int, sum_type(fd, float, double)) *zp = &y;
	float a = foo1(y);
	product_type(iSid_, int, sum_type(id, int, double)) *wp = &y; /* { dg-warning "incompatible pointer type" } */
	float b = foo1(y);
	product_type(iSid_, int, sum_type(id, int, double)) w = *wp;
	(void)a; (void)b; (void)z; (void)zp; (void)w; (void)wp;
}

int main()
{
	test1();
}

