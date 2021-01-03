/* PR c++/80560 - warn on undefined memory operations involving non-trivial
   types
   { dg-do compile }
   { dg-options "-Wclass-memaccess -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern "C"
{
void* memcpy (void*, const void*, size_t);
void* memmove (void*, const void*, size_t);
void* mempcpy (void*, const void*, size_t);
void* memset (void*, int, size_t);
void* realloc (void*, size_t);
}

namespace std {

#if __cplusplus >= 201103L
enum class byte: unsigned char { };
#else
typedef unsigned char byte;
#endif
}

#if __cplusplus < 201103L
typedef unsigned short char16_t;
#endif

/* Ordinary bzcopy and bzero aren't recognized as special.  */
#define bcopy __builtin_bcopy
#define bzero __builtin_bzero

void sink (void*);

#define T(fn, arglist) ((fn arglist), sink (p))

#if !defined TEST || TEST == TEST_TRIVIAL

/* Trivial can be manipulated by raw memory functions.  */
struct Trivial
{
  int i; unsigned bf: 1; char *s; char a[4];

  // Non-copy assignment doesn't make the class non-trivial or not
  // trivially assignable.
  Trivial& operator= (int);

  // Likewise, template assignment doesn't make the class non-trivial
  // or not trivially assignable.
  template <class U>
  Trivial& operator= (U);
};

void test (Trivial *p, void *q, int x)
{
  const size_t n = x;

  T (bzero, (p, 1));
  T (bzero, (p, n));
  T (bzero, (p, sizeof *p));
  T (bzero, (q, 1));
  T (bzero, (q, n));
  T (bzero, (q, sizeof *p));

  T (bcopy, (p, q, 1));
  T (bcopy, (p, q, n));
  T (bcopy, (p, q, sizeof *p));
  T (bcopy, (q, p, 1));
  T (bcopy, (q, p, n));
  T (bcopy, (q, p, sizeof *p));

  T (memcpy, (p, q, 1));
  T (memcpy, (p, q, n));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (q, p, 1));
  T (memcpy, (q, p, n));
  T (memcpy, (q, p, sizeof *p));

  T (memset, (p, 0, 1));
  T (memset, (p, 0, n));
  T (memset, (p, 0, sizeof *p));
  T (memset, (q, 0, 1));
  T (memset, (q, 0, n));
  T (memset, (q, 0, sizeof *p));

  T (memset, (p, 1, 1));
  T (memset, (p, 1, n));
  T (memset, (p, 1, sizeof *p));
  T (memset, (q, 1, 1));
  T (memset, (q, 1, n));
  T (memset, (q, 1, sizeof *p));

  T (memset, (p, x, 1));
  T (memset, (p, x, n));
  T (memset, (p, x, sizeof *p));
  T (memset, (q, x, 1));
  T (memset, (q, x, n));
  T (memset, (q, x, sizeof *p));

  T (memmove, (p, q, 1));
  T (memmove, (p, q, n));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (q, p, 1));
  T (memmove, (q, p, n));
  T (memmove, (q, p, sizeof *p));

  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));

  T (q = realloc, (q, 1));
  T (q = realloc, (q, n));
  T (q = realloc, (q, sizeof *p));
}

#endif

#if !defined TEST || TEST == TEST_TRIVIAL_ACCESS

/* TrivialAccess can be manipulated by raw memory functions in contexts
   that have access to the trivial specia functions.  */
struct TrivialAccess
{
  int i; unsigned bf: 1; char *s; char a[4];

private:
  TrivialAccess () = default;
  TrivialAccess (const TrivialAccess&) = default;
protected:
  TrivialAccess& operator= (const TrivialAccess&) = default;

  void test_member (const TrivialAccess*, int);

  friend void test_friend (TrivialAccess*, const TrivialAccess*, int);
};

void test (TrivialAccess *p, const TrivialAccess *q, int i)
{
  void *pv;
  (void)&pv;

  /* Verify that a warning is issued when the copy ctor and copy
     assignment are inaccessible.  */
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (bcopy, (q, p, sizeof *p));     // { dg-warning "bcopy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (pv = realloc, (p, sizeof *p)); // { dg-warning "realloc" }
}

void test_friend (TrivialAccess *p, const TrivialAccess *q, int i)
{
  void *pv;
  (void)&pv;

  /* Verify that no warning is issued when the otherwise inaccessible
     copy ctor and copy assignment can be accessed within the current
     context.  */
  T (bzero, (p, sizeof *p));
  T (bcopy, (q, p, sizeof *p));
  T (memcpy, (p, q, sizeof *p));
  T (memset, (p, i, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (pv = realloc, (p, sizeof *p));
}

void TrivialAccess::test_member (const TrivialAccess *q, int i)
{
  void *pv;
  (void)&pv;

  TrivialAccess *p = this;

  /* Verify that no warning is issued when the otherwise inaccessible
     copy ctor and copy assignment can be accessed within the current
     context.  */
  T (bzero, (p, sizeof *p));
  T (bcopy, (q, p, sizeof *p));
  T (memcpy, (p, q, sizeof *p));
  T (memset, (p, i, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (pv = realloc, (p, sizeof *p));
}

#endif

#if !defined TEST || TEST == TEST_HAS_DEFAULT

/* HasDefault is trivially copyable but should be initialized by
   the ctor, not bzero or memset.  */
struct HasDefault { char a[4]; HasDefault (); };

void test (HasDefault *p, const HasDefault &x,
	   void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // HasDefault is neither trivial nor standard-layout.  The warning
  // should mention the former since it's more permissive than the latter
  // and so more informative.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero(\[^\n\r\]*). clearing an object of non-trivial type .struct HasDefault.; use assignment or value-initialization instead" }

  T (memset, (p, 0, sizeof *p));    // { dg-warning ".void\\* memset(\[^\n\r\]*). clearing an object of non-trivial type .struct HasDefault.; use assignment or value-initialization instead" }

  T (memset, (p, 1, sizeof *p));    // { dg-warning ".void\\* memset(\[^\n\r\]*). writing to an object of non-trivial type .struct HasDefault.; use assignment instead" }

  T (memset, (p, i, sizeof *p));    // { dg-warning ".void\\* memset(\[^\n\r\]*). writing to an object of non-trivial type .struct HasDefault.; use assignment instead" }

  // Copying from another object of the same type is fine.
  T (bcopy, (&x, p, sizeof *p));
  T (bcopy, (&x, p, n));

  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, &x, n));

  // Copying from a void* or character buffer is also fine.
  T (bcopy, (q, p, sizeof *p));
  T (bcopy, (q, p, n));
  T (bcopy, (s, p, sizeof *p));
  T (bcopy, (s, p, n));
  T (bcopy, (b, p, n));

  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, q, n));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, s, n));
  T (memcpy, (p, b, n));

  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, q, n));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, s, n));
  T (memmove, (p, b, n));

  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, q, n));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, s, n));
  T (mempcpy, (p, b, n));

  // ...but partial copies are diagnosed.
  T (memcpy, (p, &x, 1));   // { dg-warning "writing to an object of a non-trivial type .struct HasDefault. leaves 3 bytes unchanged" } */
  T (memmove, (p, q, 2));   // { dg-warning "writing to an object of a non-trivial type .struct HasDefault. leaves 2 bytes unchanged" } */
  T (mempcpy, (p, q, 3));   // { dg-warning "writing to an object of a non-trivial type .struct HasDefault. leaves 1 byte unchanged" } */

  // Otherwise, copying from an object of an unrelated type is diagnosed.
  T (memcpy, (p, ia, sizeof *p));  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). copying an object of non-trivial type .struct HasDefault. from an array of .const int." }
  extern long *ip;
  T (memcpy, (p, ip, sizeof *p));  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). copying an object of non-trivial type .struct HasDefault. from an array of .long." }
  T (memcpy, (p, ss, sizeof *p));  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). copying an object of non-trivial type .struct HasDefault. from an array of .const signed char." }
  T (memcpy, (p, ws, sizeof *p));  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). copying an object of non-trivial type .struct HasDefault. from an array of .const \(char16_t\|unsigned short\)." }

  T (memmove, (p, ia, sizeof *p)); // { dg-warning ".void\\* memmove(\[^\n\r\]*). copying an object of non-trivial type .struct HasDefault. from an array of .const int." }

  T (mempcpy, (p, ia, sizeof *p)); // { dg-warning ".void\\* mempcpy(\[^\n\r\]*). copying an object of non-trivial type .struct HasDefault. from an array of .const int." }

  // Reallocating is the same as calling memcpy except that only
  // shrinking reallocation is diagnosed.
  T (q = realloc, (p, 1));   // { dg-warning "moving an object of non-trivial type .struct HasDefault. and size 4 into a region of size 1" }
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
  T (q = realloc, (p, sizeof *p + 1));
}

#endif

#if !defined TEST || TEST == TEST_HAS_TEMPLATE_DEFAULT

/* HasTemplateDefault should be initialized by means of the ctor,
   not zeroed out by bzero/memset.  */
struct HasTemplateDefault
{
  template <class U>
  HasTemplateDefault (U);
};

void test (HasTemplateDefault *p, const HasTemplateDefault &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because value initialization is
  // invalid (the template ctor makes default ctor unavailable).
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is okay.
  T (bcopy, (&x, p, sizeof *p));
  T (bcopy, (q, p, sizeof *p));
  T (bcopy, (s, p, sizeof *p));
  T (bcopy, (b, p, sizeof *p));
  T (bcopy, (ss, p, sizeof *p));    // { dg-warning "bcopy" }
  T (bcopy, (ws, p, sizeof *p));    // { dg-warning "bcopy" }
  T (bcopy, (ia, p, sizeof *p));    // { dg-warning "bcopy" }

  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
}

#endif

#if !defined TEST || TEST == TEST_HAS_COPY

/* HasCopy should be copied using the copy ctor or assignment, not
   by memcpy or memmove.  Since it's non-trivial, it should not be zeroed
   out by bzero/memset either and should instead use assignment and/or
   value initialization.  */
struct HasCopy { int i; HasCopy (const HasCopy&); };

void test (HasCopy *p, const HasCopy &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because value initialization is invalid
  // (the copy ctor makes no default ctor unavailable).  Since the type
  // has no default ctor verify that the suggested alternative does not
  // include value-initialization.
  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of non-trivial type .struct HasCopy.; use assignment instead" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (bcopy, (&x, p, sizeof *p));    // { dg-warning "bcopy" }
  T (bcopy, (q, p, sizeof *p));     // { dg-warning "bcopy" }
  T (bcopy, (s, p, sizeof *p));     // { dg-warning "bcopy" }
  T (bcopy, (b, p, sizeof *p));     // { dg-warning "bcopy" }
  T (bcopy, (ss, p, sizeof *p));    // { dg-warning "bcopy" }
  T (bcopy, (ws, p, sizeof *p));    // { dg-warning "bcopy" }
  T (bcopy, (ia, p, sizeof *p));    // { dg-warning "bcopy" }

  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_DEFAULT_AND_COPY

/* HasDefaultAndCopy is like HasCopy above but its default ctor takes
   a default argument to verify that the suggested alternative offered
   by the warning includes the default ctor (i.e., the test verifies
   that the default ctor is recognized as such despite taking an argument.  */

struct HasDefaultAndCopy
{
  HasDefaultAndCopy (int = 0);   // default ctor
  HasDefaultAndCopy (const HasDefaultAndCopy&);
};

void test (HasDefaultAndCopy *p, const HasDefaultAndCopy &x)
{
  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of non-trivial type .struct HasDefaultAndCopy.; use assignment or value-initialization instead" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "clearing an object of non-trivial type .struct HasDefaultAndCopy.; use assignment or value-initialization instead" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_PRIVATE_COPY

/* HasPrivateCopy cannot be copied using memcpy or memmove.  Since it's
   non-trivial, it it should not be zeroed out by bzero/memset either
   and should instead use assignment and/or value initialization.  */
struct HasPrivateCopy {
  int i;
private:
  HasPrivateCopy (const HasPrivateCopy&);
};

void test (HasPrivateCopy *p, const HasPrivateCopy &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because value initialization is
  // invalid (the copy ctor makes no default ctor unavailable).
  // Verify also that the suggestion offers assignment but not
  // value initialization (since the lattare is not available).
  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of non-trivial type .struct HasPrivateCopy.; use assignment instead" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of non-trivially copyable type .struct HasPrivateCopy.; use copy-assignment instead" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_DTOR

/* HasDtor should be initialized using aggregate or memberwise intialization,
   not bzero or memset.  */
struct HasDtor { int i; ~HasDtor (); };

void test (HasDtor *p, const HasDtor &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed only because it's difficult not to.
  // Otherwise, a class that's non-trivial only because it has
  // a non-trivial dtor can be safely zeroed out (that's what
  // value-initializing it does).
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed simply because
  // a class with a user-defined dtor is not trivially copyable.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_DELETED_DTOR

// HasDeletedDtor is trivial so clearing and copying it is okay.
// Relocation would bypass the deleted dtor and so it's diagnosed.

struct HasDeletedDtor
{
  int i;
  ~HasDeletedDtor () = delete;
};

void test (HasDeletedDtor *p, const HasDeletedDtor &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  T (memset, (p, 1, sizeof *p));
  T (memset, (p, i, sizeof *p));

  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));
  T (memcpy, (p, ws, sizeof *p));
  T (memcpy, (p, ia, sizeof *p));

  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));
  T (memmove, (p, ws, sizeof *p));
  T (memmove, (p, ia, sizeof *p));

  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, ss, sizeof *p));
  T (mempcpy, (p, ws, sizeof *p));
  T (mempcpy, (p, ia, sizeof *p));

  // Reallocating is diagnosed.
  T (q = realloc, (p, 1));          // { dg-warning "moving an object of type .struct HasDeletedDtor. with deleted destructor" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_PRIVATE_DTOR

// Unlike HasDeletedDtor, HasPrivateDtor is okay to zero-out and copy
// but not relocate because doing so would bypass the deleted dtor..

struct HasPrivateDtor
{
  int i;
private:
  ~HasPrivateDtor ();
};

void test (HasPrivateDtor *p, const HasPrivateDtor &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of non-trivial type .struct HasPrivateDtor.; use assignment or value-initialization instead" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "writing to an object of non-trivially copyable type .struct HasPrivateDtor.; use copy-assignment or copy-initialization instead" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is diagnosed.
  T (q = realloc, (p, 1));          // { dg-warning "moving an object of non-trivially copyable type .struct HasPrivateDtor." }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_COPY_ASSIGN

/* HasCopyAssign should be copied using the copy ctor or assignment, not
   by memcpy or memmove.  */
struct HasCopyAssign { void operator= (HasCopyAssign&); };

void test (HasCopyAssign *p, const HasCopyAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it when used with an existing
  // (already constructed) object in lieu of assigning a new value
  // to it would bypass the user-defined assignment.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_MOVE_ASSIGN

/* Like HasCopyAssign, HasMoveAssign should be copied using the copy
   ctor or assignment, not by memcpy or memmove.  */
struct HasMoveAssign
{
#if __cplusplus > 199711L
  void operator= (HasMoveAssign&&);
#else
  // C++ 98 has no reference references.  Simply repeat the HasCopyAssign
  // test to avoid having to add a conditional to every dg-warning directive.
  void operator= (const HasMoveAssign&);
#endif
};

void test (HasMoveAssign *p, const HasMoveAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it when used with an existing
  // (already constructed) object in lieu of assigning a new value
  // to it would bypass the user-defined assignment.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_TRIVIAL_COPY_HAS_MOVE_ASSIGN

/* TrivialCopyHasMoveAssign should be copied using the copy ctor
   or assignment, not by memcpy or memmove.  */
struct TrivialCopyHasMoveAssign
{
  typedef TrivialCopyHasMoveAssign Self;

  Self& operator= (const Self&) = default;

#if __cplusplus > 199711L
  Self& operator= (Self&&);
#else
  // C++ 98 has no reference references.  Fake the test by adding
  // a non-const overload of the assignment operator (which should
  // have the same effect).
  Self& operator= (Self&);
#endif
};

void test (TrivialCopyHasMoveAssign *p, const TrivialCopyHasMoveAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it when used with an existing
  // (already constructed) object in lieu of assigning a new value
  // to it would bypass the user-defined assignment.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_TRIVIAL_MOVE_HAS_COPY_ASSIGN

/* TrivialMoveNontrivialCopyAssign should be copied using the copy ctor
   or assignment, not by memcpy or memmove.  */
struct TrivialMoveNontrivialCopyAssign
{
  typedef TrivialMoveNontrivialCopyAssign Self;

  Self& operator= (const Self&);
#if __cplusplus > 199711L
  // C++ 98 has no reference references.  Fake the test by simply
  // not declaring the move assignment.
  Self& operator= (Self&&) = default;
#endif
};

void test (TrivialMoveNontrivialCopyAssign *p,
	   const TrivialMoveNontrivialCopyAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it when used with an existing
  // (already constructed) object in lieu of assigning a new value
  // to it would bypass the user-defined assignment.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_TRIVIAL_ASSIGN_REF_OVERLOAD

/* TrivialAssignRefOverload is a trivial type.  */
struct TrivialAssignRefOverload {
  int i;
  typedef TrivialAssignRefOverload Self;

  Self& operator= (Self&) = default;
  Self& operator= (const Self&) = delete;
  Self& operator= (volatile Self&) = delete;
  Self& operator= (const volatile Self&) = delete;
};

void test (TrivialAssignRefOverload *p, const TrivialAssignRefOverload &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  T (memset, (p, 1, sizeof *p));
  T (memset, (p, i, sizeof *p));

  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));
  T (memcpy, (p, ws, sizeof *p));
  T (memcpy, (p, ia, sizeof *p));

  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));
  T (memmove, (p, ws, sizeof *p));
  T (memmove, (p, ia, sizeof *p));

  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, ss, sizeof *p));
  T (mempcpy, (p, ws, sizeof *p));
  T (mempcpy, (p, ia, sizeof *p));

  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
}

#endif

#if !defined TEST || TEST == TEST_TRIVIAL_ASSIGN_CSTREF_OVERLOAD

/* TrivialAssignCstOverload is a trivial type.  */
struct TrivialAssignCstRefOverload {
  int i;
  typedef TrivialAssignCstRefOverload Self;

  Self& operator= (Self&) = delete;
  Self& operator= (const Self&) = default;
  Self& operator= (volatile Self&) = delete;
  Self& operator= (const volatile Self&) = delete;
};

void test (TrivialAssignCstRefOverload *p,
	   const TrivialAssignCstRefOverload &x,
	   const void *q, const unsigned char *s, std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  T (memset, (p, 1, sizeof *p));
  T (memset, (p, i, sizeof *p));

  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));
  T (memcpy, (p, ws, sizeof *p));
  T (memcpy, (p, ia, sizeof *p));

  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));
  T (memmove, (p, ws, sizeof *p));
  T (memmove, (p, ia, sizeof *p));

  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, ss, sizeof *p));
  T (mempcpy, (p, ws, sizeof *p));
  T (mempcpy, (p, ia, sizeof *p));

  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
}

#endif

#if !defined TEST || TEST == TEST_TRIVIAL_REF_HAS_VOLREF_ASSIGN

struct TrivialRefHasVolRefAssign
{
  typedef TrivialRefHasVolRefAssign Self;

  Self& operator= (Self&) = default;
  Self& operator= (volatile Self&);
};

void test (TrivialRefHasVolRefAssign *p,
	   const TrivialRefHasVolRefAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it when used with an existing
  // (already constructed) object in lieu of assigning a new value
  // to it would bypass the user-defined assignment.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_VOLREF_ASSIGN

struct HasVolRefAssign {
  int i;
  typedef HasVolRefAssign Self;

  Self& operator= (volatile Self&);
};

void test (HasVolRefAssign *p, const HasVolRefAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it when used with an existing
  // (already constructed) object in lieu of assigning a new value
  // to it would bypass the user-defined assignment.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying from an object of any type is diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_VIRTUALS

/* HasVirtuals should only be manipulated by the special member functions
   and not by bzero, memcpy, or any other raw memory function. Doing
   otherwse might corrupt the the vtable pointer.  */
struct HasVirtuals { int i; virtual void foo (); };

void test (HasVirtuals *p, const HasVirtuals &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because it corrupts the vtable.
  T (bzero, (p, sizeof *p));        // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying is diagnosed because when used to initialize an object
  // could incorrectly initialize the vtable.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_HAS_CONST_DATA

/* HasConstData should only be initialized using aggregate initializatoon
   and not cleared by bzero, or copied into using memcpy.  Since it's not
   assignable allowing, raw memory functions to write into it would defeat
   const-correctness.  */
struct HasConstData { const char a[4]; };

void test (HasConstData *p, const HasConstData &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // The following is ill-formed because HasConstData's cannot
  // be assigned (the assignment is implicitly deleted).  For
  // that reason all raw memory operations are diagnosed.
  // *p = x;

  // Zeroing out is diagnosed because if used with an existing
  // (already initialized) object could break const correctness.
  // Since the default ctor and copy assignment are both deleted,
  // verify that they're not suggested as a possible alternative.
  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of type .struct HasConstData. with no trivial copy-assignment \\\[" "c++ 11 and later" { target { c++11 } } }
  // { dg-warning "clearing an object of type .struct HasConstData. with no trivial copy-assignment" "c++ 98" { target { c++98_only } } .-1 }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Copying is also diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "writing to an object of type .struct HasConstData. with no trivial copy-assignment; use copy-initialization instead" "c++ 11 and later" { target { c++11 } } }
  // { dg-warning "writing to an object of type .struct HasConstData. with no trivial copy-assignment" "c++ 98" { target { c++98_only } } .-1 }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is not diagnosed except in C++ 98 due to a bug.
  T (q = realloc, (p, 1));          // { dg-warning "moving an object of non-trivially copyable type .struct HasConstData.; use .new. and .delete. instead" "c++98" { target { c++98_only } } }
  T (q = realloc, (p, n));          // { dg-warning "realloc" "c++98" { target { c++98_only } } }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" "c++98" { target { c++98_only } } }
}

#endif

#if !defined TEST || TEST == TEST_HAS_REFERENCE

/* HasReference should only be initialized using aggregate initializatoon
   and not cleared by bzero, or copied into using memcpy.  Since it's not
   assignable, allowing raw memory functions to write into it could
   corrupt the reference.  */
struct HasReference { int &ci; };

void test (HasReference *p, const HasReference &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Similarly to HasConstData, the following is ill-formed because
  // Hasreference cannot be assigned (the assignment is implicitly
  // deleted).  For that reason all raw memory operations are diagnosed.
  // *p = x;

  // Zeroing out is diagnosed because if used with an existing
  // (already initialized) object would invalidate the reference.
  // Since copy-assignment is deleted verify it's not suggested
  // as an alternative.  (C++ 11 and later only; C++ 98 is broken).
  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of type .struct HasReference. with no trivial copy-assignment \\\[" "c++ 11 and later" { target { c++11 } } }
  // { dg-warning "clearing an object of type .struct HasReference. with no trivial copy-assignment" "c++ 98" { target { c++98_only } } .-1 }
  T (bzero, (p, n));                // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 0, n));            // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, n));            // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, n));            // { dg-warning "memset" }

  // Copying is also diagnosed.
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning "writing to an object of type .struct HasReference. with no trivial copy-assignment; use copy-initialization instead" "c++ 11 and later" { target { c++11 } } }
  // { dg-warning "writing to an object of type .struct HasReference. with no trivial copy-assignment" "c++ 98" { target { c++98_only } } .-1 }
  T (memcpy, (p, &x, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, q, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, q, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ss, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, n));           // { dg-warning "memcpy" }

  T (memmove, (p, &x, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }

  T (mempcpy, (p, &x, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }

  // Reallocating is not diagnosed because a type with a reference
  // is (perhaps surprisingly) trivially copyable.  It is diagnosed
  // in C++ 98 because of a bug, but it seems like it should be
  // diagnosed in all modes.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" "c++ 98" { target { c++98_only } } }
  T (q = realloc, (p, n));          // { dg-warning "realloc" "c++ 98" { target { c++98_only } } }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" "c++ 98" { target { c++98_only } } }
}

#endif

#if !defined TEST || TEST == TEST_HAS_MEM_DATA_PTR

/* HasMemDataPtr should only be initialized using aggregate initializatoon
   and not cleared by bzero or written into using memset because its
   representation is different from ordinary scalars (a null member data
   pointer is all ones).  It can be copied into using memcpy from an object
   of the same type or from a character buffer.  */
struct HasMemDataPtr { int HasMemDataPtr::*p; };

void test (HasMemDataPtr *p, const HasMemDataPtr &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is diagnosed because a null member data pointer has
  // a representation that's all bits set.
  T (bzero, (p, sizeof *p));        // { dg-warning "clearing an object of type .struct HasMemDataPtr. containing a pointer-to-member" }
  T (bzero, (p, n));                // { dg-warning "bzero" }
  T (memset, (p, 0, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 0, n));            // { dg-warning "memset" }
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, 1, n));            // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, n));            // { dg-warning "memset" }

  // Copying is not diagnosed.
  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, &x, n));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, q, n));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, s, n));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, b, n));
  T (memcpy, (p, ss, sizeof *p));
  T (memcpy, (p, ss, n));
  T (memcpy, (p, ws, sizeof *p));
  T (memcpy, (p, ws, n));
  T (memcpy, (p, ia, sizeof *p));
  T (memcpy, (p, ia, n));

  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));
  T (memmove, (p, ws, sizeof *p));
  T (memmove, (p, ia, sizeof *p));

  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, ss, sizeof *p));
  T (mempcpy, (p, ws, sizeof *p));
  T (mempcpy, (p, ia, sizeof *p));

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
  T (q = realloc, (p, sizeof *p + 1));
}

#endif

#if !defined TEST || TEST == TEST_HAS_SOME_PRIVATE_DATA

/* HasSomePrivateData can be initialized using value initialization
   and should not be written to using memset with a non-zero argument.
   Doing otherwise would break encapsulation.  */
struct HasSomePrivateData { char a[2]; private: char b[2]; };

void test (HasSomePrivateData *p, const HasSomePrivateData &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is not diagnosed because it's equivalent to value
  // initialization.
  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  // Calling memset with a (possibly) non-zero argument is diagnosed
  // because it breaks encapsulation.
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Calling memcpy to copy from an object of the same type or from
  // a character or void buffer is not diagnosed because that's what
  // copy construction and copy assignment do.
  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, &x, n));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ss, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, n));           // { dg-warning "memcpy" }

  // Same as memcpy above.
  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, &x, n));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ss, n));          // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, n));          // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, n));          // { dg-warning "memmove" }

  // Same as memcpy above.
  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, &x, n));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, q, n));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, s, n));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, b, n));
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, n));          // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy except that partial
  // copies are not diagnosed.
  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
  T (q = realloc, (p, sizeof *p + 1));
}

#endif

#if !defined TEST || TEST == TEST_HAS_SOME_PROTECTED_DATA

/* Similarly to HasSomePrivateData, HasSomeProtectedData can be
   initialized using value initialization and should not be written
   to using memset with a non-zero argument.  Doing otherwise would
   break encapsulation.  */
struct HasSomeProtectedData { char a[2]; protected: char b[2]; };

void test (HasSomeProtectedData *p, const HasSomeProtectedData &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is not diagnosed because it's equivalent to value
  // initialization.
  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  // Calling memset with a (possibly) non-zero argument is diagnosed
  // because it breaks encapsulation.
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Calling memcpy to copy from an object of the same type or from
  // a character or void buffer is not diagnosed because that's what
  // copy construction and copy assignment do.
  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, &x, n));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ss, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, n));           // { dg-warning "memcpy" }

  // Same as memcpy above.
  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, &x, n));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ss, n));          // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, n));          // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, n));          // { dg-warning "memmove" }

  // Same as memcpy above.
  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, &x, n));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, q, n));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, s, n));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, b, n));
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, n));          // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy except that partial
  // copies are not diagnosed.
  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
  T (q = realloc, (p, sizeof *p + 1));
}

#endif

#if !defined TEST || TEST == TEST_HAS_ALL_PRIVATE_DATA

/* Similarly to HasSomePrivateData, HasAllPrivateData should only be
   initialized using value initializatoon and should not be written
   to using memset with non-zero argument.  They are tested separately
   because unlike the former classes, these are standard layout.  */
struct HasAllPrivateData { private: char a[4]; };

void test (HasAllPrivateData *p, const HasAllPrivateData &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is not diagnosed because it's equivalent to value
  // initialization.
  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  // Calling memset with a (possibly) non-zero argument is diagnosed
  // because it breaks encapsulation.
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Calling memcpy to copy from an object of the same type or from
  // a character or void buffer is not diagnosed because that's what
  // copy construction and copy assignment do.
  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, &x, n));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ss, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, n));           // { dg-warning "memcpy" }

  // Same as memcpy above.
  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, &x, n));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ss, n));          // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, n));          // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, n));          // { dg-warning "memmove" }

  // Same as memcpy above.
  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, &x, n));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, q, n));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, s, n));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, b, n));
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, n));          // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy except that partial
  // copies are not diagnosed.
  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
  T (q = realloc, (p, sizeof *p + 1));
}

#endif

#if !defined TEST || TEST == TEST_HAS_ALL_PROTECTED_DATA

/* Similarly to HasSomeProtectedData, HasAllProtectedData should only
   be initialized using value initializatoon and should not be written
   to using memset with non-zero argument.  They are tested separately
   because unlike the former classes, these are standard layout.  */
struct HasAllProtectedData { protected: char a[4]; };

void test (HasAllProtectedData *p, const HasAllProtectedData &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // Zeroing out is not diagnosed because it's equivalent to value
  // initialization.
  T (bzero, (p, sizeof *p));
  T (memset, (p, 0, sizeof *p));
  // Calling memset with a (possibly) non-zero argument is diagnosed
  // because it breaks encapsulation.
  T (memset, (p, 1, sizeof *p));    // { dg-warning "memset" }
  T (memset, (p, i, sizeof *p));    // { dg-warning "memset" }

  // Calling memcpy to copy from an object of the same type or from
  // a character or void buffer is not diagnosed because that's what
  // copy construction and copy assignment do.
  T (memcpy, (p, &x, sizeof *p));
  T (memcpy, (p, &x, n));
  T (memcpy, (p, q, sizeof *p));
  T (memcpy, (p, s, sizeof *p));
  T (memcpy, (p, b, sizeof *p));
  T (memcpy, (p, ss, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ss, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ws, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ws, n));           // { dg-warning "memcpy" }
  T (memcpy, (p, ia, sizeof *p));   // { dg-warning "memcpy" }
  T (memcpy, (p, ia, n));           // { dg-warning "memcpy" }

  // Same as memcpy above.
  T (memmove, (p, &x, sizeof *p));
  T (memmove, (p, &x, n));
  T (memmove, (p, q, sizeof *p));
  T (memmove, (p, s, sizeof *p));
  T (memmove, (p, b, sizeof *p));
  T (memmove, (p, ss, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ss, n));          // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ws, n));          // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p));  // { dg-warning "memmove" }
  T (memmove, (p, ia, n));          // { dg-warning "memmove" }

  // Same as memcpy above.
  T (mempcpy, (p, &x, sizeof *p));
  T (mempcpy, (p, &x, n));
  T (mempcpy, (p, q, sizeof *p));
  T (mempcpy, (p, q, n));
  T (mempcpy, (p, s, sizeof *p));
  T (mempcpy, (p, s, n));
  T (mempcpy, (p, b, sizeof *p));
  T (mempcpy, (p, b, n));
  T (mempcpy, (p, ss, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ss, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, n));          // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p));  // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, n));          // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy except that partial
  // copies are not diagnosed.
  T (q = realloc, (p, 1));
  T (q = realloc, (p, n));
  T (q = realloc, (p, sizeof *p));
  T (q = realloc, (p, sizeof *p + 1));
}

#endif

#if !defined TEST || TEST == TEST_DEFAULT_CTOR_PRIVATE_ASSIGN

/* Used to verify suggested alternatives.  */
struct HasDefaultPrivateAssign
{
  char a[4];
  HasDefaultPrivateAssign ();
private:
  void operator= (HasDefaultPrivateAssign&);
};

void test (HasDefaultPrivateAssign *p, const HasDefaultPrivateAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // HasDefaultPrivateAssign isn't trivial or assignable.  Verify
  // that the alternative suggested in the warning is to use copy or
  // default but not assignment.
  T (bzero, (p, sizeof *p));   // { dg-warning "bzero(\[^\n\r\]*). clearing an object of type .struct HasDefaultPrivateAssign. with no trivial copy-assignment; use value-initialization instead" "c++ 11 and later" { target { c++11 } } }
  // { dg-warning "bzero(\[^\n\r\]*). clearing an object of type .struct HasDefaultPrivateAssign. with no trivial copy-assignment; use value-initialization instead" "c++ 98" { target { c++98_only } } .-1 }

  T (memset, (p, 0, sizeof *p));   // { dg-warning ".void\\* memset(\[^\n\r\]*). clearing an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment; use value-initialization instead" }

  T (memset, (p, 1, sizeof *p));   // { dg-warning ".void\\* memset(\[^\n\r\]*). writing to an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment" }

  T (memset, (p, i, sizeof *p));   // { dg-warning ".void\\* memset(\[^\n\r\]*). writing to an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment" }

  // Copying from another object of the same type is diagnosed because
  // the copy assignment is inaccessible.  Verify that the suggested
  // alternative is not copy assignment (C++ 98 is busted).
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultPrivateAssign. with no trivial copy-assignment; use copy-initialization instead" "c++11 and later" { target c++11 } }
  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultPrivateAssign. with no trivial copy-assignment" "c++98" { target c++98_only } .-1 }
  T (memcpy, (p, &x, n));           // { dg-warning "memcpy" }

  // Similarly for copying from a void* or character buffer.
  T (memcpy, (p, q, sizeof *p));    // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultPrivateAssign. with no trivial copy-assignment; use copy-initialization instead" "c++11 and later" { target c++11 } }
  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultPrivateAssign. with no trivial copy-assignment" "c++98" { target c++98_only } ,-1 }
  T (memcpy, (p, q, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, n));            // { dg-warning "memcpy" }

  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, q, n));           // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, n));           // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, n));           // { dg-warning "memmove" }

  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, n));           // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, n));           // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, n));           // { dg-warning "mempcpy" }

  // Same for partial copies are diagnosed.
  T (memcpy, (p, &x, 1));   // { dg-warning "writing to an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment" } */
  T (memmove, (p, q, 2));   // { dg-warning "memmove" } */
  T (mempcpy, (p, q, 3));   // { dg-warning "mempcpy" } */

  // Otherwise, copying from an object of an unrelated type is diagnosed.
  T (memcpy, (p, ss, sizeof *p));  // { dg-warning "writing to an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment." }
  T (memcpy, (p, ws, sizeof *p));  // { dg-warning "writing to an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment." }
  T (memcpy, (p, ia, sizeof *p));  // { dg-warning "writing to an object of type .struct HasDefaultPrivateAssign. with (deleted|no trivial) copy-assignment." }
  T (memmove, (p, ss, sizeof *p)); // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p)); // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p)); // { dg-warning "memmove" }
  T (mempcpy, (p, ss, sizeof *p)); // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p)); // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p)); // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_DEFAULT_CTOR_DELETED_ASSIGN

/* Used to verify suggested alternatives.  */
struct HasDefaultDeletedAssign
{
  char a[4];
  HasDefaultDeletedAssign ();
private:
  void operator= (HasDefaultDeletedAssign&);
};

void test (HasDefaultDeletedAssign *p, const HasDefaultDeletedAssign &x,
	   const void *q, const unsigned char *s, const std::byte *b,
	   const signed char *ss, const char16_t *ws,
	   const int ia[])
{
  const int i = *ia;
  const size_t n = *ia;

  // HasDefaultDeletedAssign isn't trivial or assignable.  Verify
  // that the alternative suggested in the warning is to use copy or
  // default but not assignment.
  T (bzero, (p, sizeof *p));   // { dg-warning "bzero(\[^\n\r\]*). clearing an object of type .struct HasDefaultDeletedAssign. with no trivial copy-assignment; use value-initialization instead" "c++ 11 and later" { target { c++11 } } }
  // { dg-warning "bzero(\[^\n\r\]*). clearing an object of type .struct HasDefaultDeletedAssign. with no trivial copy-assignment; use value-initialization instead" "c++ 98" { target { c++98_only } } .-1 }

  T (memset, (p, 0, sizeof *p));   // { dg-warning ".void\\* memset(\[^\n\r\]*). clearing an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment; use value-initialization instead" }

  T (memset, (p, 1, sizeof *p));   // { dg-warning ".void\\* memset(\[^\n\r\]*). writing to an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment" }

  T (memset, (p, i, sizeof *p));   // { dg-warning ".void\\* memset(\[^\n\r\]*). writing to an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment" }

  // Copying from another object of the same type is diagnosed because
  // the copy assignment is inaccessible.  Verify that the suggested
  // alternative is not copy assignment (C++ 98 is busted).
  T (memcpy, (p, &x, sizeof *p));   // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultDeletedAssign. with no trivial copy-assignment; use copy-initialization instead" "c++11 and later" { target c++11 } }
  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultDeletedAssign. with no trivial copy-assignment" "c++98" { target c++98_only } .-1 }
  T (memcpy, (p, &x, n));           // { dg-warning "memcpy" }

  // Similarly for copying from a void* or character buffer.
  T (memcpy, (p, q, sizeof *p));    // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultDeletedAssign. with no trivial copy-assignment; use copy-initialization instead" "c++11 and later" { target c++11 } }
  // { dg-warning ".void\\* memcpy(\[^\n\r\]*). writing to an object of type .struct HasDefaultDeletedAssign. with no trivial copy-assignment" "c++98" { target c++98_only } ,-1 }
  T (memcpy, (p, q, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, s, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, s, n));            // { dg-warning "memcpy" }
  T (memcpy, (p, b, sizeof *p));    // { dg-warning "memcpy" }
  T (memcpy, (p, b, n));            // { dg-warning "memcpy" }

  T (memmove, (p, q, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, q, n));           // { dg-warning "memmove" }
  T (memmove, (p, s, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, s, n));           // { dg-warning "memmove" }
  T (memmove, (p, b, sizeof *p));   // { dg-warning "memmove" }
  T (memmove, (p, b, n));           // { dg-warning "memmove" }

  T (mempcpy, (p, q, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, q, n));           // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, s, n));           // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, sizeof *p));   // { dg-warning "mempcpy" }
  T (mempcpy, (p, b, n));           // { dg-warning "mempcpy" }

  // Same for partial copies are diagnosed.
  T (memcpy, (p, &x, 1));   // { dg-warning "writing to an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment" } */
  T (memmove, (p, q, 2));   // { dg-warning "memmove" } */
  T (mempcpy, (p, q, 3));   // { dg-warning "mempcpy" } */

  // Otherwise, copying from an object of an unrelated type is diagnosed.
  T (memcpy, (p, ss, sizeof *p));  // { dg-warning "writing to an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment." }
  T (memcpy, (p, ws, sizeof *p));  // { dg-warning "writing to an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment." }
  T (memcpy, (p, ia, sizeof *p));  // { dg-warning "writing to an object of type .struct HasDefaultDeletedAssign. with (deleted|no trivial) copy-assignment." }
  T (memmove, (p, ss, sizeof *p)); // { dg-warning "memmove" }
  T (memmove, (p, ws, sizeof *p)); // { dg-warning "memmove" }
  T (memmove, (p, ia, sizeof *p)); // { dg-warning "memmove" }
  T (mempcpy, (p, ss, sizeof *p)); // { dg-warning "mempcpy" }
  T (mempcpy, (p, ws, sizeof *p)); // { dg-warning "mempcpy" }
  T (mempcpy, (p, ia, sizeof *p)); // { dg-warning "mempcpy" }

  // Reallocating is the same as calling memcpy.
  T (q = realloc, (p, 1));          // { dg-warning "realloc" }
  T (q = realloc, (p, n));          // { dg-warning "realloc" }
  T (q = realloc, (p, sizeof *p));  // { dg-warning "realloc" }
}

#endif

#if !defined TEST || TEST == TEST_EXPRESSION

void test_expr (int i)
{
  struct TestClass { TestClass () { } };
  TestClass a, b;

  static void *p;

  T (bzero, (i < 0 ? &a : &b, 1));  // { dg-warning "bzero" }
}

#endif

#if !defined TEST || TEST == TEST_CTOR

void test_ctor ()
{
#undef T
#define T(fn, arglist) (fn arglist, sink (this))

  static void *p;

  struct TestBase
  {
    TestBase ()
    {
      /* A ctor of a base class with no virtual function can do whatever
	 it wants.  */
      T (bzero, (this, sizeof *this));
      T (memset, (this, 0, sizeof *this));
      T (memcpy, (this, p, sizeof *this));
      T (memmove, (this, p, sizeof *this));
      T (mempcpy, (this, p, sizeof *this));
    }

    ~TestBase ()
    {
      /* A dtor of a base class with no virtual function can do whatever
	 it wants.  */
      T (bzero, (this, sizeof *this));
      T (memset, (this, 0, sizeof *this));
      T (memcpy, (this, p, sizeof *this));
      T (memmove, (this, p, sizeof *this));
      T (mempcpy, (this, p, sizeof *this));
    }
  };

  struct TestBaseVtable
  {
    TestBaseVtable ()
    {
      /* A ctor of a base class with virtual function is treated
	 as an ordinary function.  */
      T (bzero, (this, sizeof *this));      // { dg-warning "bzero" }
      T (memset, (this, 0, sizeof *this));  // { dg-warning "memset" }
      T (memcpy, (this, p, sizeof *this));  // { dg-warning "memcpy" }
      T (memmove, (this, p, sizeof *this)); // { dg-warning "memmove" }
      T (mempcpy, (this, p, sizeof *this)); // { dg-warning "mempcpy" }
    }

    ~TestBaseVtable ()
    {
      /* A dtor of a base class with virtual function is treated
	 as an ordinary function.  */
      T (bzero, (this, sizeof *this));      // { dg-warning "bzero" }
      T (memset, (this, 0, sizeof *this));  // { dg-warning "memset" }
      T (memcpy, (this, p, sizeof *this));  // { dg-warning "memcpy" }
      T (memmove, (this, p, sizeof *this)); // { dg-warning "memmove" }
      T (mempcpy, (this, p, sizeof *this)); // { dg-warning "mempcpy" }
    }

    virtual void foo ();
  };

  struct TestDerived: HasDefault
  {
    TestDerived ()
    {
      /* A derived class ctor is treated as an ordinary function.  */
      T (bzero, (this, sizeof *this));      // { dg-warning "bzero" }
      T (memset, (this, 0, sizeof *this));  // { dg-warning "memset" }
      T (memcpy, (this, p, sizeof *this));
      T (memmove, (this, p, sizeof *this));
      T (mempcpy, (this, p, sizeof *this));
    }
  };

  struct TestDerivedDtor: HasDefault
  {
    ~TestDerivedDtor ()
    {
      /* A derived class dtor is treated as an ordinary function though
	 it probably shouldn't be unless the base dtor is trivial.  But
	 it doesn't seem worth the trouble.  */
      T (bzero, (this, sizeof *this));      // { dg-warning "bzero" }
      T (memset, (this, 0, sizeof *this));  // { dg-warning "memset" }
      T (memcpy, (this, p, sizeof *this));  // { dg-warning "memcpy" }
      T (memmove, (this, p, sizeof *this)); // { dg-warning "memmove" }
      T (mempcpy, (this, p, sizeof *this)); // { dg-warning "mempcpy" }
    }
  };
}

#endif

// { dg-prune-output "defaulted and deleted functions" }
