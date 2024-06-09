/* { dg-do compile }
 * { dg-options "-std=c23" } */

// test for nested redefinitions of enums

void foo(void)
{
	enum e { A = 1 };
	enum e { A = 1 					/* { dg-error "redeclaration" } */
		+ 0 * sizeof(enum e { A = 1 }) };	/* { dg-error "nested redefinition" } */
							
}

typedef __SIZE_TYPE__ size_t;
enum f : typeof (sizeof (enum f : size_t { B })) { B };	/* { dg-error "nested redefinition" } */



