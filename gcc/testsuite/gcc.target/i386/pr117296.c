/* PR target/117296 */
/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-additional-options "-mtune=k6 -mstringop-strategy=libcall -ffloat-store" { target ia32 } } */

int x;

void
foo (_Complex double c)
{
 lab:;
}
