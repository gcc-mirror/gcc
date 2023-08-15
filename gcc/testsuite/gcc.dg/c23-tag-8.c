/* { dg-do compile }
   { dg-options "-std=c23" } */

void foo(void)
{
	struct bar { struct bar* next; };
	struct bar { struct bar* next; };
	struct bar { struct bar { struct bar* next; }* next; };	/* { dg-error "nested" } */
}

