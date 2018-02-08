/* Test -Wsizeof-pointer-memaccess warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wsizeof-pointer-memaccess -Wno-stringop-overflow -Wno-stringop-truncation -ftrack-macro-expansion=0" } */

#define bos(ptr) __builtin_object_size (ptr, 1)
#define bos0(ptr) __builtin_object_size (ptr, 0)

#define memset(dst, val, sz) \
  (FUNC (memset, dst, val, sz, bos (dst)), sink ((dst)))

#define memcpy(dst, src, sz) \
  (FUNC (memcpy, dst, src, sz, bos (dst)), sink ((dst)))

#define memmove(dst, src, sz) \
  (FUNC (memmove, dst, src, sz, bos (dst)), sink ((dst)))

#define mempcpy(dst, src, sz) \
  (FUNC (mempcpy, dst, src, sz, bos (dst)), sink ((dst)))

#define strncpy(dst, src, sz)				\
  (FUNC (strncpy, dst, src, sz, bos (dst)), sink (dst))

#define strncat(dst, src, sz) \
  (FUNC (strncat, dst, src, sz, bos (dst)), sink (dst))

#define stpncpy(dst, src, sz) \
  (FUNC (stpncpy, dst, src, sz, bos (dst)), sink (dst))

void sink (void*);

#define S10 "123456789"
extern char a10[10];

void test_string_literal (char *dst)
{
#define FUNC(f, d, s, n, x) __builtin_ ## f (d, s, n)

  /* It's common to call memcpy and other raw memory functions with
     size drerived from the source argument.  Verify that no warning
     is ussued for such calls.  */
  memcpy (dst, S10, sizeof S10);
  mempcpy (dst, S10, sizeof S10);
  memmove (dst, S10, sizeof S10);

  memset (dst, 0, sizeof S10);

  stpncpy (dst, S10, sizeof S10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncpy (dst, S10, sizeof S10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncat (dst, S10, sizeof S10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  /* Unlike in the cases above, even though the calls below are likely
     wrong, it's not easy to detect that  the expression (sizeof X - 1)
     involves sizeof of the source, so no warning is issued here, as
     helpful as one might be.  Whether -Wstringop-truncation is issued
     is tested elsewhere.  */
  stpncpy (dst, S10, sizeof S10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncpy (dst, S10, sizeof S10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncat (dst, S10, sizeof S10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */
}


void test_char_array (char *dst)
{
  memcpy (dst, a10, sizeof a10);
  mempcpy (dst, a10, sizeof a10);
  memmove (dst, a10, sizeof a10);

  memset (dst, 0, sizeof a10);

  stpncpy (dst, a10, sizeof a10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncpy (dst, a10, sizeof a10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncat (dst, a10, sizeof a10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  stpncpy (dst, a10, sizeof a10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncpy (dst, a10, sizeof a10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncat (dst, a10, sizeof a10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */
}


#undef FUNC
#define FUNC(f, d, s, n, os) __builtin___ ## f ## _chk (d, s, n, os)

void test_char_array_chk (char *dst)
{
  memcpy (dst, S10, sizeof S10);
  mempcpy (dst, S10, sizeof S10);
  memmove (dst, S10, sizeof S10);

  memset (dst, 0, sizeof S10);

  stpncpy (dst, S10, sizeof S10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncpy (dst, S10, sizeof S10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncat (dst, S10, sizeof S10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  stpncpy (dst, S10, sizeof S10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncpy (dst, S10, sizeof S10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncat (dst, S10, sizeof S10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */
}


void test_string_literal_chk (char *dst)
{
  memcpy (dst, a10, sizeof a10);
  mempcpy (dst, a10, sizeof a10);
  memmove (dst, a10, sizeof a10);

  memset (dst, 0, sizeof a10);

  stpncpy (dst, a10, sizeof a10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncpy (dst, a10, sizeof a10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  strncat (dst, a10, sizeof a10);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" } */

  stpncpy (dst, a10, sizeof a10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncpy (dst, a10, sizeof a10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */

  strncat (dst, a10, sizeof a10 - 1);   /* { dg-warning "\\\[-Wsizeof-pointer-memaccess]" "" { xfail *-*-* } } */
}
