/* Verify -Wstringop-overread is issued appropriately for calls to string
   functions at -O0 and without -Wall.
  { dg-do compile }
  { dg-options "-O0 -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

#define S2 "12"
#define S9 "123456789"

// <libint.h> functions.

char* gettext (const char *);

// <stdio.h> functions.

typedef struct FILE FILE;

int fputs (const char*, FILE*);
int fputs_unlocked (const char*, FILE*);

int puts (const char*);
int puts_unlocked (const char*);

// <string.h> functions.

void* memchr (const void*, int, size_t);
int memcmp (const void*, const void*, size_t);
void* memcpy (void*, const void*, size_t);
void* mempcpy (void*, const void*, size_t);
void* memmove (void*, const void*, size_t);

char* strchr (const char*, int);
char* strrchr (const char*, int);

int strcmp (const char*, const char*);
int strncmp (const char*, const char*, size_t);

char* strcat (char*, const char*);
char* stpcpy (char*, const char*);
char* strcpy (char*, const char*);
char* stpncpy (char*, const char*, size_t);
char* strncpy (char*, const char*, size_t);
char* strdup (const char*);
char* strndup (const char*, size_t);

char* strpbrk (const char*, const char*);
size_t strcspn (const char*, const char*);
size_t strspn (const char*, const char*);
char* strstr (const char*, const char*);

size_t strlen (const char*);
size_t strnlen (const char*, size_t);


extern void* malloc (size_t);

void sink (void*);


extern char *d;
extern char a0[0];

const char arr[7] = "abc\0def";

/* Unterminated array at the end of ARR above.  */
#define unterm (arr + __builtin_strlen (arr) + 1)

/* Size of the unterminated array - 1.  */
#define unterm_size (sizeof arr - __builtin_strlen (arr) - 1)

const void* nowarn_memchr (int x)
{
  const char *p1 = unterm;
  return memchr (p1, x, unterm_size);
}

const void* warn_memchr (int x)
{
  const char *p1 = unterm;
  return memchr (p1, x, unterm_size + 1);       // { dg-warning "-Wstringop-overread" }
}


void* nowarn_memcpy (void)
{
  const char *s = unterm;
  return memcpy (d, s, unterm_size);
}

void* warn_memcpy (void)
{
  const char *s = unterm;
  /* Use + 2 for an odd size to prevent the memmove --> MEM_REF transform
     from defeating the warning (for now).  */
  return memcpy (d, s, unterm_size + 2 | 1);    // { dg-warning "-Wstringop-overread" }
}


void* nowarn_mempcpy (void)
{
  const char *s = unterm;
  return mempcpy (d, s, unterm_size);
}

void* warn_mempcpy (void)
{
  const char *s = unterm;
  /* Use + 2 for an odd size to prevent the memmove --> MEM_REF transform
     from defeating the warning (for now).  */
  return mempcpy (d, s, unterm_size + 2 | 1);   // { dg-warning "-Wstringop-overread" }
}


void* nowarn_memmove (void)
{
  const char *s = unterm;
  return memmove (d, s, unterm_size);
}

void* warn_memmove (void)
{
  const char *s = unterm;
  /* Use + 2 for an odd size to prevent the memmove --> MEM_REF transform
     from defeating the warning (for now).  */
  return memmove (d, s, unterm_size + 2);       // { dg-warning "-Wstringop-overread" }
}


int nowarn_memcmp_1 (const char *p2)
{
  const char *p1 = unterm;
  return memcmp (p1, p2, unterm_size);
}

int warn_memcmp_1 (const char *p2)
{
  const char *p1 = unterm;
  return memcmp (p1, p2, unterm_size + 1);      // { dg-warning "-Wstringop-overread" }
}

int nowarn_memcmp_2 (const char *p1)
{
  const char *p2 = unterm;
  return memcmp (p1, p2, unterm_size);
}

int warn_memcmp_2 (const char *p1)
{
  const char *p2 = unterm;
  return memcmp (p1, p2, unterm_size + 1);      // { dg-warning "-Wstringop-overread" }
}


void warn_strcat (void)
{
  strcat (d, unterm);                   // { dg-warning "-Wstringop-overread" }
}

void warn_strcat_a0 (void)
{
  strcat (d, a0);                       // { dg-warning "-Wstringop-overread" }
}

void warn_strcat_end (void)
{
  const char *s = arr + sizeof arr;
  strcat (d, s);                        // { dg-warning "-Wstringop-overread" }
}

char* warn_stpcpy (void)
{
  return stpcpy (d, unterm);            // { dg-warning "-Wstringop-overread" }
}

char* warn_stpcpy_a0 (void)
{
  return stpcpy (d, a0);                // { dg-warning "-Wstringop-overread" }
}

char* warn_stpcpy_end (void)
{
  const char *s = arr + sizeof arr;
  return stpcpy (d, s);                 // { dg-warning "-Wstringop-overread" }
}

char* warn_stpcpy_malloc0 (void)
{
  char *s = malloc (0);
  sink (s);
  return stpcpy (d, s);                 // { dg-warning "-Wstringop-overread" }
}


void warn_strcpy (void)
{
  strcpy (d, unterm);                   // { dg-warning "-Wstringop-overread" }
}

void warn_strcpy_a0 (void)
{
  strcpy (d, a0);                       // { dg-warning "-Wstringop-overread" }
}

void warn_strcpy_end (void)
{
  const char *s = arr + sizeof arr;
  strcpy (d, s);                        // { dg-warning "-Wstringop-overread" }
}

void warn_strcpy_malloc0 (void)
{
  char *s = malloc (0);
  sink (s);
  strcpy (d, s);                        // { dg-warning "-Wstringop-overread" }
}


char* nowarn_stpncpy (void)
{
  const char *s = unterm;
  return stpncpy (d, s, unterm_size);
}

char* warn_stpncpy (void)
{
  const char *s = unterm;
  return stpncpy (d, s, unterm_size + 1);       // { dg-warning "-Wstringop-overread" }
}

char* warn_stpncpy_a0 (void)
{
  return stpncpy (d, a0, 3);            // { dg-warning "-Wstringop-overread" }
}

char* warn_stpncpy_end (void)
{
  const char *s = arr + sizeof arr;
  return stpncpy (d, s, sizeof arr);    // { dg-warning "-Wstringop-overread" }
}


void nowarn_strncpy (void)
{
  const char *s = unterm;
  strncpy (d, s, unterm_size);
}

void warn_strncpy (void)
{
  const char *s = unterm;
  strncpy (d, s, unterm_size + 1);      // { dg-warning "-Wstringop-overread" }
}

void warn_strncpy_a0 (void)
{
  const char *s = a0;
  strncpy (d, s, sizeof arr);            // { dg-warning "-Wstringop-overread" }
}

void warn_strncpy_end (void)
{
  const char *s = arr + sizeof arr;
  strncpy (d, s, sizeof arr);            // { dg-warning "-Wstringop-overread" }
}


int warn_strlen (void)
{
  return strlen (unterm);               // { dg-warning "-Wstringop-overread" }
}

int warn_strlen_a0 (void)
{
  return strlen (a0);                   // { dg-warning "-Wstringop-overread" }
}

int warn_strlen_end (void)
{
  const char *s = arr + sizeof arr;
  return strlen (s);                    // { dg-warning "-Wstringop-overread" }
}

int warn_strlen_malloc0 (void)
{
  char *s = malloc (0);
  sink (s);
  return strlen (s);                    // { dg-warning "-Wstringop-overread" }
}


int nowarn_strnlen (void)
{
  return strnlen (unterm, unterm_size);
}

int warn_strnlen (void)
{
  return strnlen (unterm, unterm_size + 1);   // { dg-warning "-Wstringop-overread" }
}

int warn_strnlen_end (void)
{
  const char *s = arr + sizeof arr;
  return strnlen (s, 2);                // { dg-warning "-Wstringop-overread" }
}


int warn_strcmp_1 (const char *s)
{
  return strcmp (unterm, s);            // { dg-warning "-Wstringop-overread" }
}

int warn_strcmp_2 (const char *s)
{
  return strcmp (s, unterm);            // { dg-warning "-Wstringop-overread" }
}

int warn_strcmp_2_end (const char *s)
{
  const char *t = arr + sizeof arr;
  return strcmp (s, t);                 // { dg-warning "-Wstringop-overread" }
}


int nowarn_strncmp_1 (const char *s2)
{
  const char *s1 = unterm;
  return strncmp (s1, s2, unterm_size);
}

int warn_strncmp_1 (const char *s2)
{
  const char *s1 = unterm;
  return strncmp (s1, s2, unterm_size + 1);  // { dg-warning "-Wstringop-overread" }
}

int nowarn_strncmp_2 (const char *s1)
{
  const char *s2 = unterm;
  return strncmp (s1, s2, unterm_size);
}

int warn_strncmp_2 (const char *s1)
{
  const char *s2 = unterm;
  return strncmp (s1, s2, unterm_size + 1);  // { dg-warning "-Wstringop-overread" }
}

int warn_strncmp_2_end (const char *s1)
{
  const char *s2 = arr + sizeof arr;;
  return strncmp (s1, s2, sizeof arr);  // { dg-warning "-Wstringop-overread" }
}


int nowarn_strncmp_1_s2 (void)
{
  /* Since the read is also bounded by the length of the S2 literal
     and so safe, expect no warning.  */
  const char *s = unterm;
  return strncmp (s, S2, unterm_size + 1);   // { dg-bogus "-Wstringop-overread" "pr101778" { xfail *-*-* } }
}

int warn_strncmp_2_s2 (void)
{
  /* Same as above.  */
  const char *t = unterm;
  return strncmp (S2, t, unterm_size + 1);   // { dg-bogus "-Wstringop-overread" "pr101778" { xfail *-*-* } }
}


int warn_strncmp_1_s9 (void)
{
  /* Since both the bound and the length of the S9 literal are greater
     than the size of UNNTERM the call reads past the end of the array.
     Expect a warning.  */
  const char *s1 = unterm;
  return strncmp (s1, S9, unterm_size + 1);  // { dg-warning "-Wstringop-overread" }
}

int warn_strncmp_2_s9 (void)
{
  /* Same as above.  */
  const char *s2 = unterm;
  return strncmp (S9, s2, unterm_size + 1);  // { dg-warning "-Wstringop-overread" }
}


const char* warn_strchr (int x)
{
  return strchr (unterm, x);            // { dg-warning "-Wstringop-overread" }
}

const char* warn_strchr_end (int x)
{
  const char *s = arr + sizeof arr;
  return strchr (s, x);                 // { dg-warning "-Wstringop-overread" }
}


const char* warn_strrchr (int x)
{
  return strrchr (unterm, x);           // { dg-warning "-Wstringop-overread" }
}

const char* warn_strrchr_end (int x)
{
  const char *s = arr + sizeof arr;
  return strrchr (s, x);                // { dg-warning "-Wstringop-overread" }
}


char* warn_strdup (void)
{
  return strdup (unterm);               // { dg-warning "-Wstringop-overread" }
}

char* warn_strdup_end (void)
{
  const char *s = arr + sizeof arr;
  return strdup (s);                    // { dg-warning "-Wstringop-overread" }
}


char* nowarn_strndup (void)
{
  return strndup (unterm, unterm_size);
}

char* warn_strndup (void)
{
  return strndup (unterm, unterm_size + 1); // { dg-warning "-Wstringop-overread" }
}

char* warn_strndup_end (void)
{
  const char *s = arr + sizeof arr;
  return strndup (s, sizeof arr);       // { dg-warning "-Wstringop-overread" }
}


const char* warn_strpbrk_1 (const char *s2)
{
  return strpbrk (unterm, s2);          // { dg-warning "-Wstringop-overread" }
}

const char* warn_strpbrk_2 (const char *s1)
{
  return strpbrk (s1, unterm);          // { dg-warning "-Wstringop-overread" }
}


size_t warn_strspn_1 (const char *s2)
{
  return strspn (unterm, s2);           // { dg-warning "-Wstringop-overread" }
}

size_t warn_strspn_1_end (const char *s2)
{
  const char *s1 = arr + sizeof arr;
  return strspn (s1, s2);               // { dg-warning "-Wstringop-overread" }
}

size_t warn_strspn_2 (const char *s1)
{
  return strspn (s1, unterm);           // { dg-warning "-Wstringop-overread" }
}

size_t warn_strspn_2_end (const char *s1)
{
  const char *s2 = arr + sizeof arr;
  return strspn (s1, s2);               // { dg-warning "-Wstringop-overread" }
}


size_t warn_strcspn_1 (const char *s2)
{
  return strcspn (unterm, s2);          // { dg-warning "-Wstringop-overread" }
}

size_t warn_strcspn_1_end (const char *s2)
{
  const char *s1 = arr + sizeof arr;
  return strcspn (s1, s2);              // { dg-warning "-Wstringop-overread" }
}

size_t warn_strcspn_2 (const char *s1)
{
  return strcspn (s1, unterm);          // { dg-warning "-Wstringop-overread" }
}

size_t warn_strcspn_2_end (const char *s1)
{
  const char *s2 = arr + sizeof arr;
  return strcspn (s1, s2);              // { dg-warning "-Wstringop-overread" }
}


const char* warn_strstr_1 (const char *s2)
{
  return strstr (unterm, s2);           // { dg-warning "-Wstringop-overread" }
}

const char* warn_strstr_1_end (const char *s2)
{
  const char *s1 = arr + sizeof arr;
  return strstr (s1, s2);               // { dg-warning "-Wstringop-overread" }
}


const char* warn_strstr_2 (const char *s1)
{
  return strstr (s1, unterm);           // { dg-warning "-Wstringop-overread" }
}

const char* warn_strstr_2_end (const char *s1)
{
  const char *s2 = arr + sizeof arr;
  return strstr (s1, s2);               // { dg-warning "-Wstringop-overread" }
}


void warn_puts (void)
{
  puts (unterm);                        // { dg-warning "-Wstringop-overread" }
}

void warn_puts_end (void)
{
  const char *s = arr + sizeof arr;
  puts (s);                             // { dg-warning "-Wstringop-overread" }
}


void warn_fputs (FILE *f)
{
  fputs (unterm, f);                    // { dg-warning "-Wstringop-overread" }
}

void warn_fputs_end (FILE *f)
{
  const char *s = arr + sizeof arr;
  fputs (s, f);                         // { dg-warning "-Wstringop-overread" }
}


void warn_puts_unlocked (void)
{
  puts_unlocked (unterm);               // { dg-warning "-Wstringop-overread" }
}

void warn_puts_unlocked_end (void)
{
  const char *s = arr + sizeof arr;
  puts_unlocked (s);                    // { dg-warning "-Wstringop-overread" }
}

void warn_fputs_unlocked (FILE *f)
{
  fputs_unlocked (unterm, f);           // { dg-warning "-Wstringop-overread" }
}


const char* warn_gettext (void)
{
  return gettext (unterm);              // { dg-warning "-Wstringop-overread" }
}

const char* warn_gettext_end (void)
{
  const char *s = arr + sizeof arr;
  return gettext (s);                   // { dg-warning "-Wstringop-overread" }
}
