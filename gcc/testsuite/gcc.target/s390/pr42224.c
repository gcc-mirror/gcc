/* { dg-do compile } */
/* { dg-options "-O0 -Wno-implicit-function-declaration" } */

typedef char* __char_ptr32 __attribute__ (( mode (SI) ));
typedef __char_ptr32 *__char_ptr_char_ptr32 __attribute__ ((mode (SI)));

void to_ptr32 (int x)
{
  __char_ptr32 ptr = (__char_ptr32) x;
}

void to_int (__char_ptr32 ptr)
{
  int x = (int) ptr;
}

__char_ptr_char_ptr32
to_ptr32_ptr32 (char **ptr64)
{
  int argc;
  __char_ptr_char_ptr32 short_argv;

  for (argc=0; ptr64[argc]; argc++);

  short_argv = (__char_ptr_char_ptr32) malloc32
    (sizeof (__char_ptr32) * (argc + 1));

  for (argc=0; ptr64[argc]; argc++)
    short_argv[argc] = (__char_ptr32) strdup32 (ptr64[argc]);

  short_argv[argc] = (__char_ptr32) 0;
  return short_argv;

}

