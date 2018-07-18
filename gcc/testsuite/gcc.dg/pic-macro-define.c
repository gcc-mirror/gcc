/* { dg-do run } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */

#if defined __PIC__ 
int main() {
	return 0;
}
#else
  error "NO __PIC__ DEFINED"	
#endif
