/* Further tests of [*] being rejected other that in declarations, as
   per the consensus in DR#341 that the second example there should be
   invalid (but warnings because the final wording appears to allow
   these cases).  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

void foo11a(int x[sizeof(int *(*)[*])]);	/* { dg-warning "not in a declaration" } */
void foo11b(__SIZE_TYPE__ x, int y[(__UINTPTR_TYPE__)(int (*)[*])x]);	/* { dg-warning "not in a declaration" } */
void foo11c(struct s { int (*x)[*]; } *y);	/* { dg-error "a member of a structure or union cannot have a variably modified type" "variably modified" } */
/* { dg-warning "'struct s' declared inside parameter list" "struct decl" { target *-*-* } 11 } */
/* { dg-warning "its scope is only this definition or declaration" "struct scope" { target *-*-* } 11 } */
