/* { dg-do compile } */

void f () __attribute__ ((__malloc__)); /* { dg-warning "ignored" } */

int main ()
{
	/* This used to cause an ICE.  */
	f ();
}

