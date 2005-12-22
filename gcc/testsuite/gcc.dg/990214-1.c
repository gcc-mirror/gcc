/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fPIC" } */

typedef int int64_t __attribute__ ((__mode__ (  __DI__ ))) ;
unsigned *
bar (int64_t which)
{
	switch (which & 15 ) {
	case 0 :
		break;
	case 1 :
	case 5 :
	case 2 : ;
	}

	return 0;
}
