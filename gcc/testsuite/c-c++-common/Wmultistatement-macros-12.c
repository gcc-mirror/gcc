/* PR c/81364 */
/* { dg-do compile } */
/* { dg-options "-Wmultistatement-macros" } */

#define FOO0 if (1) { } else
#define TST0 \
void bar0 (void) \
{ \
  FOO0 { } /* { dg-bogus "macro expands to multiple statements" } */ \
}
TST0

#define FOO1 for (;;)
#define TST1 \
void bar1 (void) \
{ \
  FOO1 { } /* { dg-bogus "macro expands to multiple statements" } */ \
}
TST1

#define FOO2 while (1)
#define TST2 \
void bar2 (void) \
{ \
  FOO2 { } /* { dg-bogus "macro expands to multiple statements" } */ \
}
TST2

#define FOO3 switch (1)
#define TST3 \
void bar3 (void) \
{ \
  FOO3 { } /* { dg-bogus "macro expands to multiple statements" } */ \
}
TST3

#define FOO4 if (1)
#define TST4 \
void bar4 (void) \
{ \
  FOO4 { } /* { dg-bogus "macro expands to multiple statements" } */ \
}
TST4
