// PR c++/33289
// { dg-do compile }

typedef __SIZE_TYPE__ size_t;
extern "C" int __sprintf_chk (char *__restrict, int, size_t, const char *, ...) throw ();
extern "C" int __sprintf_chk (char *__restrict, int, size_t, const char *, ...) throw ();
