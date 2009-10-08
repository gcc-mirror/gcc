/* { dg-do compile } */
/* { dg-options "-O2" } */
__inline __attribute__ ((__always_inline__)) char *
strcpy (char *__dest, __const char *__src)
{
  return __builtin___strcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}

const char* get_attr(unsigned attr)
{
    static char tmp[256];

    strcpy(tmp, "");
    return tmp;
}
