/* { dg-lto-do assemble } */
/* { dg-lto-options {{-w -flto}} }  */
void Foo(void) { char bar[1]; free(bar); }
