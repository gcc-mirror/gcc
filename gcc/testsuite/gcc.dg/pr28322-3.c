/* PR28322: ignore unknown -Wno-* if no warning is emitted.  */
/* { dg-do compile } */
/* { dg-options " -fno-foobar -mno-foobar" } */

void foo(void)
{
  int i =  1;
}
/* { dg-message "unrecognized command-line option .-fno-foobar." "f" { target *-*-* } 0 } */
/* { dg-message "unrecognized command-line option .-mno-foobar." "m" { target *-*-* } 0 } */
