/* { dg-lto-do assemble } */
/* { dg-lto-options {{-fpermissive -w -flto}} }  */
void Foo(void) { char bar[1]; free(bar); }
