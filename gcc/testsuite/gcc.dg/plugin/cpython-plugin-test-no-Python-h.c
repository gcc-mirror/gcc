/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */
/* { dg-message "'Python/C API' definitions not found. Please ensure to '#include <Python.h>'." "" { target *-*-* } 0 } */

void test_no_python_plugin ()
{
}