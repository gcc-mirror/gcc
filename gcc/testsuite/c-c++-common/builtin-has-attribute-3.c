/* Verify __builtin_has_attribute return value for functions.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-options "-Wall -Wno-narrowing -Wno-unused-local-typedefs -ftrack-macro-expansion=0" { target c++ } } 
   { dg-additional-options "-DSKIP_ALIAS" { target *-*-darwin* hppa*-*-hpux* } } 
*/

#define ATTR(...) __attribute__ ((__VA_ARGS__))

void fnone (void);

ATTR (aligned) void faligned (void);
ATTR (aligned (1)) void faligned_1 (void);
ATTR (aligned (2)) void faligned_2 (void);
ATTR (aligned (4)) void faligned_4 (void);
ATTR (aligned (8)) void faligned_8 (void);

ATTR (alloc_size (1)) void* falloc_size_1 (int, int);
ATTR (alloc_size (2)) void* falloc_size_2 (int, int);
ATTR (alloc_size (2, 4)) void* falloc_size_2_4 (int, int, int, int);

ATTR (alloc_align (1)) void* falloc_align_1 (int, int);
ATTR (alloc_align (2)) void* falloc_align_2 (int, int);
ATTR (alloc_align (1), alloc_size (2)) void* falloc_align_1_size_2 (int, int);
ATTR (alloc_align (2), alloc_size (1)) void* falloc_align_2_size_1 (int, int);

#if __cplusplus
extern "C"
#endif
ATTR (noreturn) void fnoreturn (void) { __builtin_abort (); }

#ifndef SKIP_ALIAS
ATTR (alias ("fnoreturn")) void falias (void);
#endif

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

void test_aligned (void)
{
  A (0, fnone, aligned);
  A (0, fnone, aligned (0));            /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, fnone, aligned (1));
  A (0, fnone, aligned (2));
  A (0, fnone, aligned (4));
  A (0, fnone, aligned (8));
  A (0, fnone, aligned (16));

  A (1, faligned, aligned);
  A (0, faligned, aligned (0));         /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, faligned, aligned (1));
  A (0, faligned, aligned (2));

  A (1, faligned_1, aligned);
  A (0, faligned_1, aligned (0));       /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (1, faligned_1, aligned (1));
  A (0, faligned_1, aligned (2));
  A (0, faligned_1, aligned (4));

  A (1, faligned_2, aligned);
  A (0, faligned_2, aligned (0));       /* { dg-warning "requested alignment .0. is not a positive power of 2" } */
  A (0, faligned_2, aligned (1));
  A (1, faligned_2, aligned (2));
  A (0, faligned_2, aligned (4));
}


void test_alloc_align (void)
{
  A (0, fnone, alloc_align);
  A (0, falloc_size_1, alloc_align);
  A (1, falloc_align_1, alloc_align);
  A (1, falloc_align_2, alloc_align);

  A (0, fnone, alloc_align (1));        /* { dg-warning "\\\[-Wattributes" } */
  A (0, falloc_size_1, alloc_align (1));
  A (1, falloc_align_1, alloc_align (1));
  A (0, falloc_align_2, alloc_align (1));
  A (1, falloc_align_2, alloc_align (2));
}


void test_alloc_size_malloc (void)
{
  A (0, fnone, alloc_size);
  A (0, fnone, alloc_size (1));         /* { dg-warning "\\\[-Wattributes" } */
  A (0, fnone, alloc_size (2));         /* { dg-warning "\\\[-Wattributes" } */
  A (0, falloc_align_1, alloc_size (1));
  A (0, falloc_align_2, alloc_size (1));
  A (1, falloc_size_1, alloc_size (1));
  A (0, falloc_size_1, alloc_size (2));
  A (0, falloc_size_2, alloc_size (1));
  A (1, falloc_size_2, alloc_size (2));

  A (1, falloc_size_2_4, alloc_size);
  /* It would probably make more sense to have the built-in return
     true only when both alloc_size arguments match, not just one
     or the other.  */
  A (0, falloc_size_2_4, alloc_size (1));
  A (1, falloc_size_2_4, alloc_size (2));
  A (0, falloc_size_2_4, alloc_size (3));
  A (1, falloc_size_2_4, alloc_size (4));
  A (1, falloc_size_2_4, alloc_size (2, 4));

  extern ATTR (alloc_size (3))
    void* fmalloc_size_3 (int, int, int);

  A (1, fmalloc_size_3, alloc_size);
  A (0, fmalloc_size_3, alloc_size (1));
  A (0, fmalloc_size_3, alloc_size (2));
  A (1, fmalloc_size_3, alloc_size (3));
  A (0, fmalloc_size_3, malloc);

  extern ATTR (malloc)
    void* fmalloc_size_3 (int, int, int);

  A (1, fmalloc_size_3, alloc_size (3));
  A (1, fmalloc_size_3, malloc);
}

#ifndef SKIP_ALIAS
void test_alias (void)
{
  A (0, fnoreturn, alias);
  A (1, falias, alias);
  A (1, falias, alias ("fnoreturn"));
  A (0, falias, alias ("falias"));
  A (0, falias, alias ("fnone"));
}
#endif

void test_cold_hot (void)
{
  extern ATTR (cold) void fcold (void);
  extern ATTR (hot) void fhot (void);

  A (0, fnone, cold);
  A (0, fnone, hot);

  A (1, fcold, cold);
  A (0, fcold, hot);

  A (0, fhot, cold);
  A (1, fhot, hot);
}


void test_const_leaf_pure (void)
{
  extern ATTR (const) int fconst (void);
  extern ATTR (leaf) int fleaf (void);
  extern ATTR (pure) int fpure (void);

  A (0, fnone, const);
  A (0, fnone, leaf);
  A (0, fnone, pure);

  A (1, fconst, const);
  A (0, fconst, leaf);
  A (0, fconst, pure);

  A (0, fleaf, const);
  A (1, fleaf, leaf);
  A (0, fleaf, pure);

  A (0, fpure, const);
  A (0, fpure, leaf);
  A (1, fpure, pure);

  extern ATTR (const, leaf) int fconst_leaf (void);

  A (1, fconst_leaf, const);
  A (1, fconst_leaf, leaf);

  extern ATTR (leaf, const) int fleaf_const (void);

  A (1, fleaf_const, const);
  A (1, fleaf_const, leaf);
}


void test_ctor_dtor (void)
{
  extern ATTR (constructor) void fctor (void);
  extern ATTR (destructor) void fdtor (void);
  extern ATTR (constructor, destructor) void fctor_dtor (void);

  A (0, fnone, constructor);
  A (0, fnone, destructor);

  A (1, fctor, constructor);
  A (1, fdtor, destructor);

  extern ATTR (constructor) void fctor_dtor (void);
  extern ATTR (destructor) void fctor_dtor (void);
  extern ATTR (constructor, destructor) void fctor_dtor (void);

  A (1, fctor_dtor, constructor);
  A (1, fctor_dtor, destructor);
}


void test_externally_visible (void)
{
  extern void fexternally_visible (void);

  A (0, fexternally_visible, externally_visible);

  extern ATTR (externally_visible) void fexternally_visible (void);

  A (1, fexternally_visible, externally_visible);
}


void test_flatten (void)
{
  extern void fflatten (void);

  A (0, fflatten, flatten);

  extern ATTR (flatten) void fflatten (void);

  A (1, fflatten, flatten);

  extern void fflatten (void);

  A (1, fflatten, flatten);
}


ATTR (format (printf, 2, 4)) void
fformat_printf_2_3 (int, const char*, int, ...);

void test_format (void)
{
  A (0, fnone, format);
  A (0, fnone, format (printf));
  A (0, fnone, format (printf, 2));
}


inline void finline (void) { }
inline ATTR (always_inline) void falways_inline (void) { }
inline ATTR (always_inline, gnu_inline) void falways_gnu_inline (void) { }
ATTR (noinline) void fnoinline () { }

void test_inline (void)
{
  A (0, fnone, always_inline);
  A (0, fnone, gnu_inline);
  A (0, fnone, noinline);

  A (0, finline, always_inline);
  A (0, finline, gnu_inline);
  A (0, finline, noinline);

  A (1, falways_inline, always_inline);
  A (0, falways_inline, gnu_inline);
  A (0, falways_inline, noinline);

  A (1, falways_gnu_inline, always_inline);
  A (1, falways_gnu_inline, gnu_inline);
  A (0, falways_gnu_inline, noinline);

  A (0, fnoinline, always_inline);
  A (0, fnoinline, gnu_inline);
  A (1, fnoinline, noinline);
}


ATTR (no_instrument_function) void fno_instrument (void);

ATTR (visibility ("default")) void fdefault (void);
ATTR (visibility ("hidden")) void fhidden (void);
ATTR (visibility ("internal")) void finternal (void);
ATTR (visibility ("protected")) void fprotected (void);

void test_visibility (void)
{
  A (0, fnone, visibility ("default"));
  A (0, fnone, visibility ("hidden"));
  A (0, fnone, visibility ("internal"));
  A (0, fnone, visibility ("protected"));

  A (1, fdefault, visibility ("default"));
  A (0, fdefault, visibility ("hidden"));
  A (0, fdefault, visibility ("internal"));
  A (0, fdefault, visibility ("protected"));

  A (0, fhidden, visibility ("default"));
  A (1, fhidden, visibility ("hidden"));
  A (0, fhidden, visibility ("internal"));
  A (0, fhidden, visibility ("protected"));

  A (0, finternal, visibility ("default"));
  A (0, finternal, visibility ("hidden"));
  A (1, finternal, visibility ("internal"));
  A (0, finternal, visibility ("protected"));

  A (0, fprotected, visibility ("default"));
  A (0, fprotected, visibility ("hidden"));
  A (0, fprotected, visibility ("internal"));
  A (1, fprotected, visibility ("protected"));
}

/* { dg-prune-output "specifies less restrictive attribute" } */
