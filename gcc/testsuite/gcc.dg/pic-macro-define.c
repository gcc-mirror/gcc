/* { dg-do run { target "i?86-*-*-darwin" powerpc*-*-darwin* } } */
/* { dg-options "-fPIC" } */

#if defined __PIC__ 
int main() {
	return 0;
}
#else
  error "NO __PIC__ DEFINED"	
#endif
