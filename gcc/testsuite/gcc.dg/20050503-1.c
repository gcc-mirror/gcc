/* PR middle-end/21265
   Test whether tail call information is propagated through builtin
   expanders.  */
/* { dg-do compile } */
/* { dg-skip-if "" { { i?86-*-* x86_64-*-* } && { ia32 && { ! nonpic } } } } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

#define F(n, rettype, name, args, callargs) \
extern rettype name args;				\
rettype test##n args					\
{							\
  return name callargs;					\
}
#define F1(n, rettype, name, t1, callargs) \
  F (n, rettype, name, (t1 a1), callargs)
#define F2(n, rettype, name, t1, t2, callargs) \
  F (n, rettype, name, (t1 a1, t2 a2), callargs)
#define F3(n, rettype, name, t1, t2, t3, callargs) \
  F (n, rettype, name, (t1 a1, t2 a2, t3 a3), callargs)

F3 (1a, void *, memcpy, void *, const void *, size_t, (a1, a2, a3))
F3 (1b, void *, memcpy, void *, const void *, size_t, (a1, a2, 10))
F3 (2a, void *, mempcpy, void *, const void *, size_t, (a1, a2, a3))
F3 (2b, void *, mempcpy, void *, const void *, size_t, (a1, a2, 10))
F3 (3a, void *, memmove, void *, const void *, size_t, (a1, a2, a3))
F3 (3b, void *, memmove, void *, const void *, size_t, (a1, "abcdefghijklmno", a3))
F3 (4a, void *, memset, void *, int, size_t, (a1, a2, a3))
F3 (4b, void *, memset, void *, int, size_t, (a1, a2, 156))
F3 (4c, void *, memset, void *, int, size_t, (a1, 0, a3))
F3 (4d, void *, memset, void *, int, size_t, (a1, 0, 10000))
F3 (5a, int, memcmp, const void *, const void *, size_t, (a1, a2, a3))
F3 (5b, int, memcmp, const void *, const void *, size_t, (a1, "abcdefghijkl", a3))
F2 (6, char *, strcpy, char *, const char *, (a1, a2))
F2 (7, char *, stpcpy, char *, const char *, (a1, a2))
F3 (8, char *, strncpy, char *, const char *, size_t, (a1, a2, a3))
F3 (9, char *, stpncpy, char *, const char *, size_t, (a1, a2, a3))
F2 (10, char *, strcat, char *, const char *, (a1, a2))
F3 (11, char *, strncat, char *, const char *, size_t, (a1, a2, a3))
F1 (12a, size_t, strlen, const char *, (a1))
F1 (12b, size_t, strlen, const char *, ("foobar"))
F2 (13a, int, strcmp, const char *, const char *, (a1, a2))
F2 (13b, int, strcmp, const char *, const char *, (a1, "abcdefghijklm"))
F3 (14a, int, strncmp, const char *, const char *, size_t, (a1, a2, a3))
F3 (14b, int, strncmp, const char *, const char *, size_t, (a1, "abcdefghijklm", 10))
F2 (15, char *, strchr, const char *, int, (a1, a2))

/* All the calls above should be tail call optimized on i?86/x86-64.  */
/* { dg-final { scan-assembler-not "call" { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } } */
