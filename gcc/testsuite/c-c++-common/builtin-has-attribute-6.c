/* PR c/88383 - ICE calling _builtin_has_attribute(r, aligned(N)))
   on an overaligned reference r
   PR c/89288 - ICE in tree_code_size, at tree.c:865
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-options "-Wall -Wno-narrowing -Wno-unused -ftrack-macro-expansion=0" { target c++ } }  */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

typedef ATTR (aligned (8)) int Int8;

/* The attribute applies to the array, not to the type of its elements.  */
extern ATTR (aligned (8)) char i8arr[];

/* The attribute applies to the pointer, not to the type it points to.  */
extern ATTR (aligned (8)) int *ptr;
extern Int8 *i8ptr;

#if __cplusplus

/* Similarly here, the attribute applies to the reference, not to its type.  */
extern ATTR (aligned (8)) int &ref;
extern Int8 &i8ref;

#else

/* Fake references in C.  */
extern ATTR (aligned (8)) int ref;
Int8 i8ref;

#endif

void test (void)
{
  /* Verify that the built-in detects the attribute on the array. */
  A (1, i8arr, aligned);
  A (0, i8arr, aligned (1));
  A (0, i8arr, aligned (2));
  A (0, i8arr, aligned (4));
  A (1, i8arr, aligned (8));
  A (0, i8arr, aligned (16));

  A (0, i8arr + 1, aligned);
  A (0, i8arr + 2, aligned (1));
  A (0, i8arr + 3, aligned (8));

  /* Verify the builtin detects the absence of the attribute on
     the elements.  */
  A (0, i8arr[0], aligned);
  A (0, *i8arr,   aligned);

  /* Verify that the built-in doesn't confuse the attribute on
     the pointer type with that to the pointed to type.  This
     also exercises PR c/89288.  */
  A (0, (Int8*)0, aligned);
  A (0, (int*)0,  aligned);
  A (0, (void*)0, aligned);
  A (0, 0,        aligned);

  /* Verify that the built-in detects the attribute on the pointer
     itself. */
  A (1, ptr, aligned);
  A (0, ptr, aligned (1));
  A (0, ptr, aligned (2));
  A (0, ptr, aligned (4));
  A (1, ptr, aligned (8));
  A (0, ptr, aligned (16));

  A (0, ptr + 1, aligned);
  A (0, ptr + 2, aligned (1));
  A (0, ptr + 3, aligned (8));

  /* The pointed to type is not declared with attribute aligned.  */
  A (0, *ptr, aligned);
  A (0, *ptr, aligned (1));
  A (0, *ptr, aligned (2));
  A (0, *ptr, aligned (4));
  A (0, *ptr, aligned (8));
  A (0, *ptr, aligned (16));

  A (0, *ptr + 1, aligned);
  A (0, *ptr + 2, aligned (1));
  A (0, *ptr + 3, aligned (8));

  /* Verify that the built-in correctly detects the attribute on
     the type of the lvalue referenced by the pointer. */
  A (0, i8ptr,     aligned);
  A (0, i8ptr,     aligned (8));
  A (0, i8ptr + 1, aligned);
  A (0, i8ptr + 3, aligned (8));
  A (1, *i8ptr,    aligned);
  A (0, *i8ptr,    aligned (1));
  A (0, *i8ptr,    aligned (2));
  A (0, *i8ptr,    aligned (4));
  A (1, *i8ptr,    aligned (8));
  A (0, *i8ptr,    aligned (16));

  /* The reference itself is declared aligned, even though the type
     it refers to isn't.  But see PR c++/88362.  */
  A (1, ref, aligned);
  A (0, ref, aligned (1));
  A (0, ref, aligned (2));
  A (0, ref, aligned (4));
  A (1, ref, aligned (8));
  A (0, ref, aligned (16));

  /* Also verify that assignment expressions are accepted.  */
  A (0, ref = 1,  aligned);
  A (0, ref += 2, aligned (1));
  A (0, ref /= 3, aligned (8));

}
