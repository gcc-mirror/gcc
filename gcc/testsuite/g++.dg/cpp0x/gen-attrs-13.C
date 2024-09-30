// { dg-additional-options "-Wno-c++11-extensions" }
// PR c++/13854

extern char *rindex [[gnu::__pure__]] (__const char *__s, int __c) throw ();
extern char *rindex [[gnu::__pure__]] (__const char *__s, int __c) throw ();
