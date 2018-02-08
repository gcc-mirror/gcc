// { dg-options "-Wsequence-point -fdiagnostics-show-caret" }
// { dg-require-effective-target alloca }

void *libiberty_concat_ptr;
extern unsigned long concat_length (const char *, ...);
extern char *concat_copy2 (const char *, ...);

#define ACONCAT(ACONCAT_PARAMS) \
  (libiberty_concat_ptr = (char *) ALLOCA (concat_length ACONCAT_PARAMS + 1), /* { dg-warning "may be undefined" } */ \
   concat_copy2 ACONCAT_PARAMS)

/* Arbitrary content here.
   In PR c++/70105, this was >500 lines of source.
   This should not be printed.  */

# define ALLOCA(x) __builtin_alloca(x)

int strlen (const char *);
void *get_identifier (const char *);
void *get_identifier_with_length (const char *, int);

#define GET_IDENTIFIER(STR) \
  (__builtin_constant_p (STR)				\
    ? get_identifier_with_length ((STR), strlen (STR))  \
    : get_identifier (STR))

void *test(void)
{
  int *i;
  return GET_IDENTIFIER (ACONCAT (("foo")));
}

/* { dg-begin-multiline-output "" }
   (libiberty_concat_ptr = (char *) ALLOCA (concat_length ACONCAT_PARAMS + 1),
                         ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
     ? get_identifier_with_length ((STR), strlen (STR))  \
                                                  ^~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   return GET_IDENTIFIER (ACONCAT (("foo")));
                          ^~~~~~~
   { dg-end-multiline-output "" } */
