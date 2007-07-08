/* PR c/18809 */
/* Origin: Andrew Pinski <pinskia@gcc.gnu.org> */

/* { dg-do compile } */

void foo(enum E e) {}   /* { dg-error "forward ref" "forward" } */
			/* { dg-warning "declared" "declared" { target *-*-* } 6 } */
			/* { dg-warning "scope" "scope" { target *-*-* } 6 } */
			/* { dg-error "incomplete" "incomplete" { target *-*-* } 6 } */
void bar() { foo(0); }  /* { dg-error "formal" } */
