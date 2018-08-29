/* PR middle-end/54486 */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */
/* { dg-skip-if "no POSIX printf" { *-*-mingw* } } */

#ifdef __cplusplus
extern "C" {
#endif
typedef __SIZE_TYPE__ size_t;
extern int printf (const char *, ...);
extern size_t strspn (const char *, const char *);
extern size_t strcspn (const char *, const char *);
extern size_t strlen (const char *);
#ifdef __cplusplus
}
#endif

void
foo (void)
{
  printf ("%zu\n", strspn ("abc", "abcdefg"));
  printf ("%zu\n", (size_t) strspn ("abc", "abcdefg"));
  printf ("%zu\n", __builtin_strspn ("abc", "abcdefg"));
  printf ("%zu\n", (size_t) __builtin_strspn ("abc", "abcdefg"));
  printf ("%zu\n", strcspn ("abc", "abcdefg"));
  printf ("%zu\n", (size_t) strcspn ("abc", "abcdefg"));
  printf ("%zu\n", __builtin_strcspn ("abc", "abcdefg"));
  printf ("%zu\n", (size_t) __builtin_strcspn ("abc", "abcdefg"));
  printf ("%zu\n", strlen ("abc"));
  printf ("%zu\n", (size_t) strlen ("abc"));
  printf ("%zu\n", __builtin_strlen ("abc"));
  printf ("%zu\n", (size_t) __builtin_strlen ("abc"));
}
