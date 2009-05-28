typedef __SIZE_TYPE__ size_t;
typedef struct
{
}
_G_fpos_t;
extern int printf (__const char *__restrict __format, ...);
extern size_t strlen (__const char *__s) __attribute__ ((__nothrow__))
  __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
typedef struct rend_service_descriptor_t
{
  int patchlevel;
  char status_tag[32];
}
tor_version_t;
test_dir_format (void)
{
  tor_version_t ver1;
  {
    long v2 = (long) (ver1.patchlevel);
  }
  {
    const char *v1 = (""), *v2 = (ver1.status_tag);
    if (!__extension__ (
			 {
			 size_t __s1_len, __s2_len;
			 (__builtin_constant_p (v1)
			  && (__s1_len = strlen (v1), __s2_len =
			      (!((size_t) (const void *) ((v1) + 1) -
				 (size_t) (const void *) (v1) == 1)
			       || __s1_len >= 4)
			      &&
			      (!((size_t) (const void *) ((v2) + 1) -
				 (size_t) (const void *) (v2) == 1)
			       || __s2_len >= 4)) ? __builtin_strcmp (v1,
								      v2)
			  : (__builtin_constant_p (v1)
			     && ((size_t) (const void *) ((v1) + 1) -
				 __s1_len < 4) ? (__builtin_constant_p (v2)
						  &&
						  ((size_t) (const void *)
						   (size_t) (const void
							     *) (v2) ==
						   1) ? __builtin_strcmp (v1,
									  v2)
						  : (__extension__ (
											 {
											 __const
											 char
											 *__s2
											 =
											 (__const
											  char
											  *)
											 (v2);
											 register
											 __result
											 =
											 (((__const unsigned char *) (__const char *) (v1))[0] - __s2[0]); if (__s1_len > 0 && __result == 0)
											 {
											 }
											 __result;}
    ))):					     
			     (__builtin_constant_p (v2)
			      && ((size_t) (const void *) ((v2) + 1) -
				  __s2_len < 4) ? (__builtin_constant_p (v1)
						   && ((size_t) (const void *)
						       1) ?
						   __builtin_strcmp (v1,
								     v2)
						   : (__extension__ (
									      {
									      __const
									      char
									      *__s1
									      =
									      (__const
									       char
									       *)
									      (__const
									       *)
									      (v1);
									      register
									      __result
									      =
									      ((__const unsigned char *) (__const char *) (v2))[0]; if (__s2_len > 0 && __result == 0)
									      {
									      if
									      (__s2_len
									       ==
									       0)
									      {
									      }
									      }
									      __result;}
  ))):						      __builtin_strcmp (v1,
									v2))));}
	))
      {
	printf (".");
      }
  }
}
