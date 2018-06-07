// PR c++/80955
// { dg-do run { target c++11 } }

extern "C" int sprintf (char *s, const char *format, ...);
extern "C" int strcmp (const char *s1, const char *s2);

#define __PRI64_PREFIX        "l"
#define PRId64         __PRI64_PREFIX "d"

using size_t = decltype(sizeof(0));
#define _zero
#define _ID _xx
int operator""_zero(const char*, size_t) { return 0; }
int operator""_ID(const char*, size_t) { return 0; }

int main()
{
  long i64 = 123;
  char buf[] = "xxxxxx"__FILE__;      // { dg-warning "invalid suffix on literal" }
  sprintf(buf, "%"PRId64"abc", i64);  // { dg-warning "invalid suffix on literal" }
  return strcmp(buf, "123abc")
	 + ""_zero
	 + "bob"_zero
	 + R"#(raw
	       string)#"_zero
	 + "xx"_ID
	 + ""_ID
	 + R"AA(another
		raw
		string)AA"_ID;
}
