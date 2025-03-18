/* { dg-do run { target { lp64 || ilp32 } } } */
/* { dg-options "-O2" } */
/* { dg-output-file "dg-output-file-1-lp64.txt" { target lp64 } } */
/* { dg-output-file "dg-output-file-1-ilp32.txt" { target ilp32 } } */

int
main ()
{
  __builtin_printf ("This is a test output for %s target\n"
		    "to verify\n"
		    "dg-output-file directive\n",
		    __SIZEOF_LONG__ * __CHAR_BIT__ == 64 ? "lp64" : "ilp32");
}
