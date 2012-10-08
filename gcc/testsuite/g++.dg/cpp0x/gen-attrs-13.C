// { dg-do compile { target c++11 } }
// PR c++/13854

extern char *rindex [[gnu::__pure__]] (__const char *__s, int __c) throw ();
extern char *rindex [[gnu::__pure__]] (__const char *__s, int __c) throw ();
