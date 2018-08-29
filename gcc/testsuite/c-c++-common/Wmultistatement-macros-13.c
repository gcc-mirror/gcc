/* PR c/81448 */
/* { dg-do compile } */
/* { dg-options "-Wmultistatement-macros" } */

extern int i;

#define BAD4 i++; i++ /* { dg-warning "macro expands to multiple statements" } */
#define BAD5 i++; i++ /* { dg-warning "macro expands to multiple statements" } */
#define BAD6 i++; i++ /* { dg-warning "macro expands to multiple statements" } */
#define BAD7 i++; i++ /* { dg-warning "macro expands to multiple statements" } */
#define BAD8 i++; i++ /* { dg-warning "macro expands to multiple statements" } */
#define BAD9 i++; i++ /* { dg-warning "macro expands to multiple statements" } */
#define IF if (1) /* { dg-message "not guarded by this 'if' clause" } */
#define IF2 IF /* { dg-message "in expansion of macro .IF." } */
#define BADB7 BAD7 /* { dg-message "in expansion of macro .BAD7." } */
#define BADB8 BAD8 /* { dg-message "in expansion of macro .BAD8." } */
#define BADB9 BAD9 /* { dg-message "in expansion of macro .BAD9." } */

#define FN0				\
void fn0 (void)				\
{					\
  IF					\
    i++;				\
  return;				\
}

#define FN1				\
void fn1 (void)				\
{					\
  IF2					\
    i++;				\
  return;				\
}

#define FN2				\
void fn2 (void)				\
{					\
  if (1)				\
    i++;				\
  return;				\
}

#define TOP FN3
#define FN3				\
void fn3 (void)				\
{					\
  IF					\
    i++;				\
  return;				\
}

#define TOP2 FN4 /* { dg-message "in expansion of macro .FN4." } */
#define FN4				\
void fn4 (void)				\
{					\
  IF2 /* { dg-message "in expansion of macro .IF2." } */ \
    BAD4; /* { dg-message "in expansion of macro .BAD4." } */ \
}

#define FN5				\
void fn5 (void)				\
{					\
  IF /* { dg-message "in expansion of macro .IF." } */	\
    BAD5; /* { dg-message "in expansion of macro .BAD5." } */ \
}

#define FN6				\
void fn6 (void)				\
{					\
  if (1) /* { dg-message "not guarded by this 'if' clause" } */ \
    BAD6; /* { dg-message "in expansion of macro .BAD6." } */ \
}

#define FN7				\
void fn7 (void)				\
{					\
  if (1) /* { dg-message "not guarded by this 'if' clause" } */	\
    BADB7; /* { dg-message "in expansion of macro .BADB7." } */ \
}

#define FN8				\
void fn8 (void)				\
{					\
  IF2 /* { dg-message "in expansion of macro .IF2." } */ \
    BADB8; /* { dg-message "in expansion of macro .BADB8." } */ \
}

#define FN9				\
void fn9 (void)				\
{					\
  IF /* { dg-message "in expansion of macro .IF." } */ \
    BADB9; /* { dg-message "in expansion of macro .BADB9." } */	\
}

FN0
FN1
FN2
TOP
TOP2 /* { dg-message "in expansion of macro .TOP2." } */
FN5 /* { dg-message "in expansion of macro .FN5." } */
FN6 /* { dg-message "in expansion of macro .FN6." } */
FN7 /* { dg-message "in expansion of macro .FN7." } */
FN8 /* { dg-message "in expansion of macro .FN8." } */
FN9 /* { dg-message "in expansion of macro .FN9." } */
