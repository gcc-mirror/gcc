/* PR middle-end/103770 */
/* { dg-do compile } */
/* { dg-options "" } */

struct struct_s {
	void* ptr;
	void* ptr2;
	void* ptr3;
};

struct struct_s struct_create(int N, const long vla[N]);

void fun(int N)
{
	long vla[N];
	struct struct_s st = struct_create(N, vla);
}


extern _Complex float g(int N, int dims[N]);

void f(void)
{
 	int dims[1];
	_Complex float val = g(1, dims);
}

