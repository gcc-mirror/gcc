/* This testcase used to fail because a miscompiled execute_fold_all_builtins. */

typedef __SIZE_TYPE__ size_t;
extern __inline __attribute__ ((__always_inline__)) int __attribute__
((__nothrow__)) snprintf (char *__restrict __s, size_t __n, __const char
*__restrict __fmt, ...)  {
  return __builtin___snprintf_chk (__s, __n, 2 - 1,       
__builtin_object_size (__s, 2 > 1), __fmt, __builtin_va_arg_pack ());
}
int n1, n2, n3, n4, f5, f6;
char * BackgroundGetUniqueString(void)
{
  char s[256];
  const char *chmap =
"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_";
  snprintf(s, sizeof(s),
"%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
           chmap[(n1 >> 0) & 0x3f], chmap[(n1 >> 6) & 0x3f],
           chmap[(n1 >> 12) & 0x3f], chmap[(n1 >> 18) & 0x3f],
           chmap[(n1 >> 24) & 0x3f], chmap[(n1 >> 28) & 0x3f],
           chmap[(n2 >> 0) & 0x3f], chmap[(n2 >> 6) & 0x3f],
           chmap[(n2 >> 12) & 0x3f], chmap[(n2 >> 18) & 0x3f],
           chmap[(n2 >> 24) & 0x3f], chmap[(n2 >> 28) & 0x3f],
           chmap[(n3 >> 0) & 0x3f], chmap[(n3 >> 6) & 0x3f],
           chmap[(n3 >> 12) & 0x3f], chmap[(n3 >> 18) & 0x3f],
           chmap[(n3 >> 24) & 0x3f], chmap[(n3 >> 28) & 0x3f],
           chmap[(n4 >> 0) & 0x3f], chmap[(n4 >> 6) & 0x3f],
           chmap[(n4 >> 12) & 0x3f], chmap[(n4 >> 18) & 0x3f],
           chmap[(n4 >> 24) & 0x3f], chmap[(n4 >> 28) & 0x3f],
           chmap[(f5 >> 12) & 0x3f], chmap[(f5 >> 18) & 0x3f],
           chmap[(f5 >> 24) & 0x3f], chmap[(f5 >> 28) & 0x3f],
           chmap[(f6 >> 0) & 0x3f], chmap[(f6 >> 6) & 0x3f]
           );
  return __builtin_strdup(s);
}
