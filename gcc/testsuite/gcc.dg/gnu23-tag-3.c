/* 
 * { dg-do compile } 
 * { dg-options "-Wno-vla -std=gnu23" } 
 */

// arrays in structs

void foo(int n, int m)
{
	struct f { int b; int a[n]; };
	struct f { int b; int a[n]; };	/* { dg-error "redefinition of struct or union" } */
	struct f { int b; int a[m]; };	/* { dg-error "redefinition of struct or union" } */
	struct f { int b; int a[5]; };	/* { dg-error "redefinition of struct or union" } */
	struct f { int b; int a[]; };	/* { dg-error "redefinition of struct or union" } */

	struct g { int a[n]; int b; };
	struct g { int a[n]; int b; };	/* { dg-error "redefinition of struct or union" } */
	struct g { int a[m]; int b; };	/* { dg-error "redefinition of struct or union" } */
	struct g { int a[4]; int b; };	/* { dg-error "redefinition of struct or union" } */

	struct h { int (*a)[n]; int b; };
	struct h { int (*a)[n]; int b; };	/* { dg-error "redefinition of struct or union" } */
	struct h { int (*a)[m]; int b; };	/* { dg-error "redefinition of struct or union" } */
	struct h { int (*a)[4]; int b; };	/* { dg-error "redefinition of struct or union" } */
	struct h { int (*a)[]; int b; };	/* { dg-error "redefinition of struct or union" } */
}


