/* PR middle-end/81117 - Improve buffer overflow checking in strncpy
   { dg-do compile }
   { dg-options "-O2 -Wstringop-truncation -Wno-stringop-overflow -ftrack-macro-expansion=0" }
   { dg-require-effective-target alloca } */


typedef __SIZE_TYPE__ size_t;

#if __cplusplus
extern "C" {
#endif

size_t strlen (const char*);
char* strncat (char*, const char*, size_t);
char* strncpy (char*, const char*, size_t);

#if __cplusplus
}
#endif

static size_t unsigned_value (void)
{
  extern volatile size_t unsigned_value_source;
  return unsigned_value_source;
}

static size_t unsigned_range (size_t min, size_t max)
{
  size_t val = unsigned_value ();
  return val < min || max < val ? min : val;
}

#define UR(min, max) unsigned_range (min, max)

void sink (void*);

#define S4 "123"
const char a4[] = "123";

#define CHOOSE(a, b) (unsigned_value () & 1 ? a : b)


typedef struct Dest
{
  char a5[5];
  char b7[7];
  char c3ns[3] __attribute__ ((nonstring));
} Dest;

char dst7[7];
char dst2_5[2][5];

/* Verify strncat warnings for arrays of known bounds.  */

void test_strncat_array (Dest *pd)
{
#define CAT(d, s, len) (strncat ((d), (s), (len)), sink (d))

  CAT (dst7, S4, 2);                /* { dg-warning "output truncated copying 2 bytes from a string of length 3" } */

  CAT (dst7, a4, 1);                /* { dg-warning "output truncated copying 1 byte from a string of length 3" } */

  /* There is no truncation here but possible overflow so these
     are diagnosed by -Wstringop-overflow:
     CAT (dst7, S4, 3);
     CAT (dst7, a4, 3);
  */

  CAT (pd->a5, S4, 2);              /* { dg-warning "output truncated copying 2 bytes from a string of length 3" } */
  CAT (pd->a5, S4, 1);              /* { dg-warning "output truncated copying 1 byte from a string of length 3" } */
}

/* Verify strncat warnings for arrays of known bounds and a non-const
   character count in some range.  */

void test_strncat_array_range (Dest *pd)
{
  CAT (dst7, S4, UR (0, 1));        /* { dg-warning "output truncated copying between 0 and 1 bytes from a string of length 3" } */
  CAT (dst7, S4, UR (0, 2));        /* { dg-warning "output truncated copying between 0 and 2 bytes from a string of length 3" } */
  CAT (dst7, S4, UR (1, 3));        /* { dg-warning "output truncated copying between 1 and 3 bytes from a string of length 3" } */
  CAT (dst7, S4, UR (2, 4));        /* { dg-warning "output may be truncated copying between 2 and 4 bytes from a string of length 3" } */

  CAT (dst7, S4, UR (0, 7));
  CAT (dst7, S4, UR (1, 7));
  CAT (dst7, S4, UR (6, 7));

  CAT (dst7, S4, UR (0, 99));

  CAT (dst7, S4, UR (0, 99));
}

/* Verify strncat warnings for arrays of unknown bounds.  */

void test_strncat_vla (char *d, unsigned n)
{
  CAT (d, S4, 2);                   /* { dg-warning "output truncated copying 2 bytes from a string of length 3" } */
  CAT (d, S4, 4);

  CAT (d, a4, 2);                   /* { dg-warning "output truncated copying 2 bytes from a string of length 3" } */

  /* There is no truncation here but possible overflow so these
     are diagnosed by -Wstringop-overflow:
     CAT (d, S4, 3);
     CAT (d, a4, 3);
  */
  CAT (d, a4, 4);

  char vla[n];

  CAT (vla, S4, 2);                 /* { dg-warning "output truncated copying 2 bytes from a string of length 3" } */

  CAT (vla, S4, 4);
  CAT (vla, S4, n);

  CAT (vla, a4, 2);                 /* { dg-warning "output truncated copying 2 bytes from a string of length 3" } */

  CAT (vla, a4, 4);
  CAT (vla, a4, n);

  CAT (d, vla, 1);
  CAT (d, vla, 3);
  CAT (d, vla, 4);
  CAT (d, vla, n);

  /* There is no truncation here but possible overflow so these
     are diagnosed by -Wstringop-overflow:
  CAT (vla, S4, 3);
  CAT (vla, a4, 3);
  */
}

/* Verify strncpy warnings with at least one pointer to an object
   or string of unknown size (destination) or length (source).  */

void test_strncpy_ptr (char *d, const char* s, const char *t, int i)
{
#define CPY(d, s, len) (strncpy ((d), (s), (len)), sink (d))

  /* Strncpy doesn't nul-terminate so the following is diagnosed.  */
  CPY (d, "",    0);                /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */
  CPY (d, s,     0);                /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */

  /* This is safe.  */
  CPY (d, "",    1);
  CPY (d, "",    2);

  /* This could be safe.  */
  CPY (d, s,     1);
  CPY (d, s,     2);

  /* Truncation.  */
  CPY (d, "123", 1);                /* { dg-warning ".strncpy\[^\n\r\]* output truncated copying 1 byte from a string of length 3" } */
  CPY (d, "123", 2);                /* { dg-warning ".strncpy\[^\n\r\]* output truncated copying 2 bytes from a string of length 3" } */
  CPY (d, "123", 3);                /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 3 bytes from a string of the same length" } */
  CPY (d, "123", 4);
  CPY (d, "123", 9);

  CPY (d, S4, sizeof S4);           /* Covered by -Wsizeof-pointer-memaccess.  */
  CPY (d, S4, sizeof S4 - 1);       /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 3 bytes from a string of the same length" } */

  CPY (d, a4, sizeof a4);           /* Covered by -Wsizeof-pointer-memaccess.  */
  CPY (d, a4, sizeof a4 - 1);       /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 3 bytes from a string of the same length" } */
  CPY (d, a4, sizeof a4 - 3);       /* { dg-warning ".strncpy\[^\n\r\]* output truncated copying 1 byte from a string of length 3" } */
  CPY (d, a4, sizeof a4 - 4);       /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes from a string of length 3" } */

  CPY (d, S4, strlen (S4));         /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 3 bytes from a string of the same length" } */
  /* Likely buggy but no truncation.  Diagnosed by -Wstringop-overflow.  */
  CPY (d, a4, strlen (a4) + 1);
  CPY (d, S4, strlen (S4) + i);

  CPY (d, a4, strlen (a4));         /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 3 bytes from a string of the same length" } */
  /* As above, buggy but no evidence of truncation.  */
  CPY (d, S4, strlen (S4) + 1);

  CPY (d, CHOOSE ("", "1"), 0);     /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */
  CPY (d, CHOOSE ("1", "12"), 0);   /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */

  CPY (d, CHOOSE ("", "1"), 1);     /* { dg-warning ".strncpy\[^\n\r\]* output may be truncated copying 1 byte from a string of length 1" } */
  CPY (d, CHOOSE ("1", ""), 1);     /* { dg-warning ".strncpy\[^\n\r\]* output may be truncated copying 1 byte from a string of length 1" } */
  CPY (d, CHOOSE (s, "1"), 1);      /* { dg-warning ".strncpy\[^\n\r\]* output may be truncated copying 1 byte from a string of length 1" } */
  CPY (d, CHOOSE (s, t), 1);

  CPY (d, CHOOSE ("", "1"), 2);
  CPY (d, CHOOSE ("1", ""), 2);
  CPY (d, CHOOSE ("1", "2"), 2);
  CPY (d, CHOOSE ("1", s), 2);
  CPY (d, CHOOSE (s, "1"), 2);
  CPY (d, CHOOSE (s, t), 2);

  CPY (d, CHOOSE ("", "123"), 1);   /* { dg-warning ".strncpy\[^\n\r\]* output may be truncated copying 1 byte from a string of length 3" } */
  CPY (d, CHOOSE ("1", "123"), 1);  /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 1 byte from a string of the same length" } */
  CPY (d, CHOOSE ("12", "123"), 1); /* { dg-warning ".strncpy\[^\n\r\]* output truncated copying 1 byte from a string of length 2" } */
  CPY (d, CHOOSE ("123", "12"), 1); /* { dg-warning ".strncpy\[^\n\r\]* output truncated copying 1 byte from a string of length 2" } */

  {
    signed char n = strlen (s);     /* { dg-message "length computed here" } */
    CPY (d, s, n);                  /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying as many bytes from a string as its length" } */
  }

  {
    short n = strlen (s);           /* { dg-message "length computed here" } */
    CPY (d, s, n);                  /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying as many bytes from a string as its length" } */
  }

  {
    int n = strlen (s);             /* { dg-message "length computed here" } */
    CPY (d, s, n);                  /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying as many bytes from a string as its length" } */
  }

  {
    unsigned n = strlen (s);        /* { dg-message "length computed here" } */
    CPY (d, s, n);                  /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying as many bytes from a string as its length" } */
  }

  {
    size_t n;
    n = strlen (s);                 /* { dg-message "length computed here" } */
    CPY (d, s, n);                  /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying as many bytes from a string as its length" } */
  }

  {
    size_t n;
    char *dp2 = d + 1;
    n = strlen (s);                 /* { dg-message "length computed here" } */
    CPY (dp2, s, n);                /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying as many bytes from a string as its length" } */
  }

  {
    /* The following truncates the terminating nul.  The warning should
       say that but doesn't.  */
    int n;
    n = strlen (s) - 1;
    CPY (d, s, n);                  /* { dg-warning "\\\[-Wstringop-truncation" } */
  }

  {
    /* Same as above.  */
    size_t n;
    n = strlen (s) - 1;
    CPY (d, s, n);                  /* { dg-warning "\\\[-Wstringop-truncation" } */
  }

  {
    size_t n = strlen (s) - strlen (s);
    CPY (d, s, n);                  /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */
  }

  {
    /* This use of strncpy is dubious but it's probably not worth
       worrying about (truncation may not actually take place when
       i is the result).  It is diagnosed with -Wstringop-overflow
       (although more by accident than by design).

       size_t n = i < strlen (s) ? i : strlen (s);
       CPY (d, s, n);
    */
  }
}


/* Verify strncpy warnings for arrays of known bounds.  */

void test_strncpy_array (Dest *pd, int i, const char* s)
{
#undef CPY
#define CPY(d, s, len) (strncpy ((d), (s), (len)), sink (d))

  CPY (dst7, s, 7);                 /* { dg-warning "specified bound 7 equals destination size" } */
  CPY (dst7, s, sizeof dst7);       /* { dg-warning "specified bound 7 equals destination size" } */

  CPY (dst2_5[0], s, sizeof dst2_5[0]); /* { dg-warning "specified bound 5 equals destination size" "bug 77293" } */
  CPY (dst2_5[1], s, sizeof dst2_5[1]); /* { dg-warning "specified bound 5 equals destination size" } */

  /* Verify that copies that nul-terminate are not diagnosed.  */
  CPY (dst7,     "",       sizeof dst7);
  CPY (dst7 + 6, "",       sizeof dst7 - 6);
  CPY (dst7,     "1",      sizeof dst7);
  CPY (dst7 + 1, "1",      sizeof dst7 - 1);
  CPY (dst7,     "123456", sizeof dst7);
  CPY (dst7 + 1, "12345",  sizeof dst7 - 1);

  CPY (dst7 + i, s,        6);
  CPY (dst7 + i, s,        7);      /* { dg-warning "specified bound 7 equals destination size" } */
  /* The following two calls are diagnosed by -Wstringop-overflow.  */
  CPY (dst7 + i, s,        8);
  CPY (dst7 + i, s,        UR (8, 9));

  /* No nul-termination here.  */
  CPY (dst7 + 2, "12345",  sizeof dst7 - 2);    /* { dg-warning "output truncated before terminating nul copying 5 bytes from a string of the same length" } */

  /* Because strnlen appends as many NULs as necessary to write the specified
     number of byts the following doesn't (necessarily) truncate but rather
     overflow, and so is diagnosed by -Wstringop-overflow.  */
  CPY (dst7, s, 8);

  CPY (dst7 + 1, s, 6);             /* { dg-warning "specified bound 6 equals destination size" } */
  CPY (dst7 + 6, s, 1);             /* { dg-warning "specified bound 1 equals destination size" } */

  CPY (pd->a5, s, 5);               /* { dg-warning "specified bound 5 equals destination size" } */
  CPY (pd->a5, s, sizeof pd->a5);   /* { dg-warning "specified bound 5 equals destination size" } */

  CPY (pd->a5 + i, s, sizeof pd->a5);   /* { dg-warning "specified bound 5 equals destination size" "member array" } */

  /* Verify that a copy that nul-terminates is not diagnosed.  */
  CPY (pd->a5, "1234", sizeof pd->a5);

  /* Same above, diagnosed by -Wstringop-overflow.  */
  CPY (pd->a5, s, 6);

  /* Exercise destination with attribute "nonstring".  */
  CPY (pd->c3ns, "", 3);
  CPY (pd->c3ns, "", 1);
  /* It could be argued that truncation in the literal case should be
     diagnosed even for non-strings.  Using strncpy in this case is
     pointless and should be replaced with memcpy.  But it would likely
     be viewed as a false positive.  */
  CPY (pd->c3ns, "12", 1);
  CPY (pd->c3ns, "12", 2);
  CPY (pd->c3ns, "12", 3);
  CPY (pd->c3ns, "123", 3);
  CPY (pd->c3ns, s, 3);
  CPY (pd->c3ns, s, sizeof pd->c3ns);

  /* Verify that the idiom of calling strncpy with a bound equal to
     the size of the destination (and thus potentially without NUL-
     terminating it) immediately followed by setting the last element
     of the array to NUL is not diagnosed.  */
  {
    /* This might be better written using memcpy() but it's safe so
       it shouldn't be diagnosed.  */
    strncpy (dst7, "0123456", sizeof dst7);   /* { dg-bogus "\\\[-Wstringop-truncation]" } */
    dst7[sizeof dst7 - 1] = '\0';
    sink (dst7);
  }

  {
    const char a[] = "0123456789";
    strncpy (dst7, a, sizeof dst7);
    dst7[sizeof dst7 - 1] = '\0';
    sink (dst7);
  }

  {
    strncpy (dst7, s, sizeof dst7);
    dst7[sizeof dst7 - 1] = '\0';
    sink (dst7);
  }

  {
    strncpy (pd->a5, "01234", sizeof pd->a5);   /* { dg-bogus "\\\[-Wstringop-truncation]" } */
    pd->a5[sizeof pd->a5 - 1] = '\0';
    sink (pd);
  }

  {
    strncpy (pd->a5, s, sizeof pd->a5);
    pd->a5[sizeof pd->a5 - 1] = '\0';
    sink (pd);
  }

  {
    unsigned n = 7;
    char *p = (char*)__builtin_malloc (n);
    strncpy (p, s, n);
    p[n - 1] = '\0';
    sink (p);
  }

  {
    /* This should be diagnosed because the NUL-termination doesn't
       immediately follow the strncpy call (sink may expect pd->a5
       to be NUL-terminated).  */
    strncpy (pd->a5, s, sizeof pd->a5); /* { dg-warning "specified bound 5 equals destination size" } */
    sink (pd);
    pd->a5[sizeof pd->a5] = '\0';
    sink (pd);
  }
}

typedef struct Flex
{
  size_t n;
  char a0[0];
  char ax[];
} Flex;

extern char array[];

/* Verify that no warning is issued for array of unknown bound, flexible
   array members, or zero-length arrays, except when the source is definitely
   truncated.  */

void test_strncpy_flexarray (Flex *pf, const char* s)
{
#undef CPY
#define CPY(d, s, len) (strncpy ((d), (s), (len)), sink (d))

  CPY (array, "12345", 7);
  CPY (array, "12345", 123);

  CPY (array, s, 7);
  CPY (array, s, 123);

  CPY (pf->a0, s, 1);
  CPY (pf->a0, s, 1234);

  CPY (pf->a0, "",      1);
  CPY (pf->a0, "12345", 5);          /* { dg-warning "output truncated before terminating nul copying 5 bytes from a string of the same length" } */
  CPY (pf->a0, "12345", 1234);

  CPY (pf->ax, s, 5);
  CPY (pf->ax, s, 12345);

  CPY (pf->ax, "1234", 5);
  CPY (pf->ax, "12345", 5);         /* { dg-warning "output truncated before terminating nul copying 5 bytes from a string of the same length" } */
  CPY (pf->ax, "12345", 12345);
}

/* Verify warnings for dynamically allocated objects.  */

void test_strncpy_alloc (const char* s)
{
  size_t n = 7;
  char *d = (char *)__builtin_malloc (n);

  CPY (d, s, n);                    /* { dg-warning "specified bound 7 equals destination size" } */

  Dest *pd = (Dest *)__builtin_malloc (sizeof *pd * n);
  CPY (pd->a5, s, 5);               /* { dg-warning "specified bound 5 equals destination size" } */
  CPY (pd->a5, s, sizeof pd->a5);   /* { dg-warning "specified bound 5 equals destination size" } */
}

/* Verify warnings for VLAs.  */

void test_strncpy_vla (unsigned n, const char* s)
{
  char vla[n];
  CPY (vla, s, 0);                  /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */

  CPY (vla, s, 1);
  CPY (vla, s, 2);
  CPY (vla, s, n);

  CPY (vla, "", 0);                 /* { dg-warning ".strncpy\[^\n\r\]* destination unchanged after copying no bytes" } */
  CPY (vla, "", 1);
  CPY (vla, S4, 3);                 /* { dg-warning ".strncpy\[^\n\r\]* output truncated before terminating nul copying 3 bytes from a string of the same length" } */
  CPY (vla, S4, n);
}
