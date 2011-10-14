/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2" } */

typedef __builtin_va_list __va_list;
typedef __va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
struct MSVCRT__iobuf { };
typedef struct MSVCRT__iobuf MSVCRT_FILE;
typedef union _printf_arg { } printf_arg;
MSVCRT_FILE MSVCRT__iob[20];
int pf_print_a (va_list *);
int __attribute__((__cdecl__))
MSVCRT_vfprintf_s(MSVCRT_FILE* file, const char *format, va_list valist)
{
  if(!((file != ((void *)0))
       || (MSVCRT__invalid_parameter(((void *)0), ((void *)0),
				     ((void *)0), 0, 0),0)))
      return -1;
  return pf_printf_a(&valist);
}
int __attribute__((__cdecl__))
MSVCRT_vprintf_s(const char *format, va_list valist)
{
  return MSVCRT_vfprintf_s((MSVCRT__iob+1),format,valist);
}
int __attribute__((__cdecl__))
MSVCRT_fprintf_s(MSVCRT_FILE* file, const char *format, ...)
{
  va_list valist;
  va_start (valist, format);
  return MSVCRT_vfprintf_s(file, format, valist);
}
