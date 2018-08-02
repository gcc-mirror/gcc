/* { dg-do compile } */

#define B _Pragma("GCC diagnostic push") \
	  _Pragma("GCC diagnostic ignored \"-Wattributes\"")
#define E _Pragma("GCC diagnostic pop")

#define X() B int __attribute((unknown_attr)) x; E /* { dg-bogus "attribute directive ignored" } */
#define Y   B int __attribute((unknown_attr)) y; E /* { dg-bogus "attribute directive ignored" } */

void test1(void)
{
    X()  /* { dg-bogus "in expansion of macro" } */
    Y    /* { dg-bogus "in expansion of macro" } */
}
