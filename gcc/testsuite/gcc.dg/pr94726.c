/* { dg-do compile } */
/* { dg-options "-O2" } */
typedef unsigned int type __attribute__ ( ( vector_size ( 2*sizeof(int) ) ) ) ; 
type a , b; 
/* { dg-message "note: previous declaration" "previous declaration" { target *-*-* } .-1 } */
void foo ( void ) { 
	type var = { 2 , 2 } ; 
	b = __builtin_shuffle ( a , var ) ;
} 

void * a [ ] = { } ; /* { dg-error "conflicting types" } */
