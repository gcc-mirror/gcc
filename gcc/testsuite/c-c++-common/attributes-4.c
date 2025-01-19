/* PR c/88363 - alloc_align attribute doesn't accept enumerated arguments
   Verify that attribute positional arguments can refer to all C integer
   types except _Bool in both C and C++.
   { dg-do compile }
   { dg-options "-Wall" }
   { dg-options "-Wall -Wno-c++-compat" { target c } } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#if __cplusplus == 199711L
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
#elif !__cplusplus
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
typedef __WCHAR_TYPE__  wchar_t;
#endif

enum A { A0 };

ATTR (alloc_align (1)) void* falloc_align_char (char);
ATTR (alloc_align (1)) void* falloc_align_char16 (char16_t);
ATTR (alloc_align (1)) void* falloc_align_char32 (char32_t);
ATTR (alloc_align (1)) void* falloc_align_wchar (wchar_t);
/* Using an enum might make sense in an API that limits the alignments
   it accepts to just the set of the defined enumerators.   */
ATTR (alloc_align (1)) void* falloc_align_enum (enum A);
#ifdef __SIZEOF_INT128__
ATTR (alloc_align (1)) void* falloc_align_int128 (__int128_t);
#endif


ATTR (alloc_align (1)) void* falloc_size_char (char);
ATTR (alloc_size (1)) void* falloc_size_char16 (char16_t);
ATTR (alloc_size (1)) void* falloc_size_char32 (char32_t);
ATTR (alloc_size (1)) void* falloc_size_wchar (wchar_t);
ATTR (alloc_size (1)) void* falloc_size_enum (enum A);
#ifdef __SIZEOF_INT128__
ATTR (alloc_align (1)) void* falloc_size_int128 (__int128_t);
#endif


typedef struct { int i; } S;

/* Using bool is most likely a bug and so diagnosed even though
   it could be accepted.  None of the other types makes sense.  */
ATTR (alloc_align (1)) void* falloc_align_bool (bool);      /* { dg-warning "attribute argument value .1. refers to parameter type .\(_Bool|bool\)" } */
ATTR (alloc_align (1)) void* falloc_align_float (float);    /* { dg-warning "attribute argument value .1. refers to parameter type .float" } */
ATTR (alloc_align (1)) void* falloc_align_voidp (void*);    /* { dg-warning "attribute argument value .1. refers to parameter type .void ?\\\*" } */
ATTR (alloc_align (1)) void* falloc_align_struct (S);       /* { dg-warning "attribute argument value .1. refers to parameter type .S" } */
