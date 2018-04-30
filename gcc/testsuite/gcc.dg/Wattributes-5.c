/* { dg-do compile } */
/* { dg-options -Wattributes } */

void __attribute__((_foobar)) foo() { }		/* { dg-warning "attribute directive ignored" } */
void __attribute__((_xformat__)) foo2() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((xformat__)) foo3() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((__xformat)) foo4() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((_)) foo5() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((_)) foo6() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((__)) foo7() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((___)) foo8() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((____)) foo9() { }	/* { dg-warning "attribute directive ignored" } */
void __attribute__((_____)) foo10() { }	/* { dg-warning "attribute directive ignored" } */
