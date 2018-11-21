/* Verify __builtin_has_attribute error handling.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" }  */

#define ATTR(list) __attribute__ (list)

void fnone (void);

ATTR ((aligned)) void faligned (void);
ATTR ((aligned (8))) void faligned_8 (void);

#define has_attr(x, attr)   __builtin_has_attribute (x, attr)

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(has_attr (sym, attr) == expect)]


int b;

/* Exercise syntactically invalid arguments.  */

void test_bad_arguments (void)
{
  b = __builtin_has_attribute ();            /* { dg-error "expected \(primary-\)?expression|expected .,." } */
  b = __builtin_has_attribute (1);           /* { dg-error "expected .,." } */
  b = __builtin_has_attribute (void);        /* { dg-error "expected .,." } */
  b = __builtin_has_attribute (foo);         /* { dg-error ".foo. \(undeclared|was not declared\)" } */
  /* { dg-error "expected .,." "missing comma" { target *-*-* } .-1 } */

  /* Verify the implementationm doesn't ICE.  */
  b = __builtin_has_attribute (foobar, aligned);  /* { dg-error ".foobar. \(undeclared|was not declared\)" } */

  b = __builtin_has_attribute (1, 2, 3);     /* { dg-error "expected identifier" } */
  b = __builtin_has_attribute (int, 1 + 2);  /* { dg-error "expected identifier" } */
  b = __builtin_has_attribute (2, "aligned"); /* { dg-error "expected identifier" } */
}

/* Exercise syntactically valid arguments applied in invalid ways.  */

void test_invalid_arguments (void)
{
  b = has_attr (fnone, align);        /* { dg-error "unknown attribute .align." } */
  b = has_attr (b, aligned__);        /* { dg-error "unknown attribute .aligned__." } */
  b = has_attr (fnone, aligned (3));  /* { dg-error "alignment .3. is not a positive power of 2" } */

  /* Verify the out-of-bounds arguments are diagnosed and the result
     of the built-in is false.  */
  A (0, fnone, alloc_size (1));       /* { dg-warning "\\\[-Wattributes]" } */
  A (0, fnone, alloc_size (2));       /* { dg-warning "\\\[-Wattributes]" } */

  A (0, int, alloc_size (1));         /* { dg-warning ".alloc_size. attribute only applies to function types" } */

  int i = 1;
  A (0, i, alloc_size (1));           /* { dg-warning ".alloc_size. attribute only applies to function types" } */

  A (0, faligned_8, aligned (i));     /* { dg-error "alignment is not an integer constant" } */

  typedef ATTR ((aligned (2))) char CA2;
  b = has_attr (CA2[2], aligned);     /* { dg-error "alignment of array elements is greater than element size" } */
}
