/* { dg-options "-fcondition-coverage" } */

/* https://gcc.gnu.org/pipermail/gcc-patches/2022-April/592927.html */
char trim_filename_name;
int r;

void trim_filename() {
    if (trim_filename_name)
	r = 123;
    while (trim_filename_name)
	;
}

int main ()
{
}
