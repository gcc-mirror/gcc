/* PR middle-end/15666 */
/* Origin: <hongjiu.lu@intel.com> */

extern __inline int
foo (const char *__s, char __reject)
{
  return __builtin_strchr (__s, __reject) != ((void *)0);
}

extern char *bar (__const char *__s, int __c);
extern __typeof (bar) strchr __asm__ ("bar");
