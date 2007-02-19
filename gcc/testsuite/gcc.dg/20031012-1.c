/* { dg-options "-Waddress" } */
/* Origin: Andrew Morton <akpm@osdl.org> */
/* Warn if a function addres of a non-weak function is used
   as a truth value.  */
/* See thread starting at http://gcc.gnu.org/ml/gcc/2003-10/msg00414.html  */

void foo(void)
{}

void bar(void)
{}

int main() {
	if (foo) /* { dg-warning "" } */
		bar();
	return 0;
}

