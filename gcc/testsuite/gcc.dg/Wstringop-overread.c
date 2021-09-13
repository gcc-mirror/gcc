/* Verify -Wstringop-overread is issued appropriately.
  { dg-do compile }
  { dg-options "-O2 -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

// <libint.h> functions.

char* gettext (const char *);

// <stdio.h> functions.

int puts (const char*);
int puts_unlocked (const char*);

// <string.h> functions.

char* strchr (const char*, int);

int strcmp (const char*, const char*);
int strncmp (const char*, const char*, size_t);

char* strcat (char*, const char*);
char* strcpy (char*, const char*);
char* strncpy (char*, const char*, size_t);
char* strdup (const char*);
char* strndup (const char*, size_t);

char* strpbrk (char*, const char*);
size_t strcspn (const char*, const char*);
size_t strspn (const char*, const char*);
char* strstr (char*, const char*);

size_t strlen (const char*);
size_t strnlen (const char*, size_t);


void sink (int, ...);
#define sink(...) sink (0, __VA_ARGS__)

extern char *d;
extern char a0[0];              // { dg-message "source object 'a0'" }
extern char a1[1];              // { dg-message "source object 'a1'" }
extern char a2[2];              // { dg-message "source object 'a2'" }

extern char b1[1];
extern char b2[2];
extern char bx[];

const char s0[0] = { };         // { dg-message "source object 's0'" }
const char s1[1] = "";          // { dg-message "source object 's1'" }
const char s2[2] = "1";         // { dg-message "source object 's2'" }

#define T(x) sink (0, (x))


void test_strcat_array (const char *s, int i, int i0)
{
  if (i0 < 0)
    i0 = 0;

  T (strcat (d, a0));           // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, a0 + i));       // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, a0 + i + 1));   // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }

  T (strcat (d, a0 + i0));      // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }

  T (strcat (d, a1));
  T (strcat (d, a1 + 1));       // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, a1 + i));
  T (strcat (d, a1 + i + 1));

  T (strcat (d, a1 + i0));
  T (strcat (d, a1 + i0 + 1));  // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }

  T (strcat (d, a2));
  T (strcat (d, a2 + 1));
  T (strcat (d, a2 + 2));       // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, a2 + i));
  T (strcat (d, a2 + i + 2));

  T (strcat (d, a2 + i0));
  T (strcat (d, a2 + i0 + 1));
  T (strcat (d, a2 + i0 + 2));  // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }

  // Repeat the above with the arguments reversed.

  T (strcat (a0, s));           // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }
  T (strcat (a0 + i, s));       // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }
  T (strcat (a0 + i + 1, s));   // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }

  T (strcat (a0 + i0, s));      // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }

  T (strcat (a1, s));
  T (strcat (a1 + 1, s));       // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }
  T (strcat (a1 + i, s));
  T (strcat (a1 + i + 1, s));

  T (strcat (a1 + i0, s));
  T (strcat (a1 + i0 + 1, s));  // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }

  T (strcat (a2, s));
  T (strcat (a2 + 1, s));
  T (strcat (a2 + 2, s));       // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }
  T (strcat (a2 + i, s));
  T (strcat (a2 + i + 2, s));

  T (strcat (a2 + i0, s));
  T (strcat (a2 + i0 + 1, s));
  T (strcat (a2 + i0 + 2, s));  // { dg-warning "'strcat' writing 1 or more bytes into a region of size 0" }
}

void test_strcat_literal (int i)
{
  T (strcat (d, ""));
  T (strcat (d, "" + 0));
  T (strcat (d, "" + i));

  T (strcat (d, "1"));
  T (strcat (d, "1" + 1));
  T (strcat (d, "1" + 2));      // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, "1" + i));

  T (strcat (d, "12"));
  T (strcat (d, "12" + 1));
  T (strcat (d, "12" + 2));
  T (strcat (d, "12" + 3));     // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, "12" + i));
}

void test_strcat_string (int i)
{
  T (strcat (d, s0));           // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, s0 + 1));       // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, s0 + i));       // { dg-warning "'strcat' (reading 1 or more bytes from a region of size 0|argument missing terminating nul)" }

  T (strcat (d, s1));
  T (strcat (d, s1 + 1));      // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, s1 + 2));      // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, s1 + i));

  T (strcat (d, s2));
  T (strcat (d, s2 + 1));
  T (strcat (d, s2 + 2));      // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, s2 + 3));      // { dg-warning "'strcat' reading 1 or more bytes from a region of size 0" }
  T (strcat (d, s2 + i));
}


void test_strcpy_array (int i, int i0)
{
  if (i0 < 0)
    i0 = 0;

  T (strcpy (d, a0));           // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, a0 + i));       // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, a0 + i + 1));   // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }

  T (strcpy (d, a0 + i0));      // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }

  T (strcpy (d, a1));
  T (strcpy (d, a1 + 1));       // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, a1 + i));
  T (strcpy (d, a1 + i + 1));

  T (strcpy (d, a1 + i0));
  T (strcpy (d, a1 + i0 + 1));  // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }

  T (strcpy (d, a2));
  T (strcpy (d, a2 + 1));
  T (strcpy (d, a2 + 2));       // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, a2 + i));
  T (strcpy (d, a2 + i + 2));

  T (strcpy (d, a2 + i0));
  T (strcpy (d, a2 + i0 + 1));
  T (strcpy (d, a2 + i0 + 2));  // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
}

void test_strcpy_literal (int i)
{
  T (strcpy (d, ""));
  T (strcpy (d, "" + 0));
  T (strcpy (d, "" + i));

  T (strcpy (d, "1"));
  T (strcpy (d, "1" + 1));
  T (strcpy (d, "1" + 2));      // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, "1" + i));

  T (strcpy (d, "12"));
  T (strcpy (d, "12" + 1));
  T (strcpy (d, "12" + 2));
  T (strcpy (d, "12" + 3));     // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, "12" + i));
}

void test_strcpy_string (int i)
{
  T (strcpy (d, s0));           // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, s0 + 1));       // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, s0 + i));       // { dg-warning "'strcpy' (reading 1 or more bytes from a region of size 0|argument missing terminating nul)" }

  T (strcpy (d, s1));
  T (strcpy (d, s1 + 1));      // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, s1 + 2));      // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, s1 + i));

  T (strcpy (d, s2));
  T (strcpy (d, s2 + 1));
  T (strcpy (d, s2 + 2));      // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, s2 + 3));      // { dg-warning "'strcpy' reading 1 or more bytes from a region of size 0" }
  T (strcpy (d, s2 + i));
}


void test_strncpy_array (int i)
{
  T (strncpy (d, a0, 0));
  T (strncpy (d, a0, 1));       // { dg-warning "'strncpy' reading 1 byte from a region of size 0" }
  T (strncpy (d, a0 + i, 0));
  T (strncpy (d, a0 + i, 1));   // { dg-warning "'strncpy' reading 1 byte from a region of size 0" }

  T (strncpy (d, a1, 0));
  T (strncpy (d, a1, 1));
  T (strncpy (d, a1 + 1, 0));
  T (strncpy (d, a1 + 1, 1));   // { dg-warning "'strncpy' reading 1 byte from a region of size 0" }
  T (strncpy (d, a1 + i, 0));
  T (strncpy (d, a1 + i, 1));
  T (strncpy (d, a1 + i, 2));
}


void test_strncpy_literal (int i, int i0)
{
  if (i0 < 0)
    i0 = 0;

  T (strncpy (d, "", 0));
  T (strncpy (d, "", 1));
  T (strncpy (d, "", 2));

  T (strncpy (d, "" + i, 0));
  T (strncpy (d, "" + i, 1));
  T (strncpy (d, "" + i0, 1));
  T (strncpy (d, "" + i0, 1));

  T (strncpy (d, "" + 1, 0));
  T (strncpy (d, "" + 1, 1));   // { dg-warning "'strncpy' reading 1 byte from a region of size 0" }

  T (strncpy (d, "1", 0));
  T (strncpy (d, "1" + 1, 0));
  T (strncpy (d, "1" + 1, 1));
  T (strncpy (d, "1" + 1, 2));
  T (strncpy (d, "1" + i, 2));

  T (strncpy (d, "1" + 2, 0));
  T (strncpy (d, "1" + 2, 1));  // { dg-warning "'strncpy' reading 1 byte from a region of size 0" }
}


void test_strlen_array (int i, int i0)
{
  if (i0 < 0)
    i0 = 0;

  T (strlen (a0));              // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }
  T (strlen (a0 + i));          // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }
  T (strlen (a0 + i + 1));      // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }

  T (strlen (a0 + i0));         // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }

  T (strlen (a1));
  T (strlen (a1 + 1));          // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }
  T (strlen (a1 + i));
  T (strlen (a1 + i + 1));

  T (strlen (a1 + i0));
  T (strlen (a1 + i0 + 1));     // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }

  T (strlen (a2));
  T (strlen (a2 + 1));
  T (strlen (a2 + 2));          // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }
  T (strlen (a2 + i));
  T (strlen (a2 + i + 2));

  T (strlen (a2 + i0));
  T (strlen (a2 + i0 + 1));
  T (strlen (a2 + i0 + 2));     // { dg-warning "'strlen' reading 1 or more bytes from a region of size 0" }
}


void test_strnlen_array (int i, int i0, unsigned n)
{
  if (i0 < 0)
    i0 = 0;

  T (strnlen (a0, 0));
  T (strnlen (a0, 1));          // { dg-warning "'strnlen' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }
  T (strnlen (a0, i0));
  T (strnlen (a0, i0 + 1));     // { dg-warning "'strnlen' (reading between 1 and \[0-9\]+ bytes from a region of size 0|specified bound \\\[1, \[0-9\]+\\\] exceeds source size 0)" }
  T (strnlen (a0, n));
  T (strnlen (a0 + i, 0));
  T (strnlen (a0 + i, 1));      // { dg-warning "'strnlen' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }
  T (strnlen (a0 + i, i0));
  T (strnlen (a0 + i, n));
  T (strnlen (a0 + i + 1, 0));
  T (strnlen (a0 + i + 1, 1));  // { dg-warning "'strnlen' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }

  T (strnlen (a0 + i0, 0));
  T (strnlen (a0 + i0, 1));     // { dg-warning "'strnlen' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }
  T (strnlen (a0 + i0, n));

  T (strnlen (a1, 0));
  T (strnlen (a1, 1));
  T (strnlen (a1, 2));          // { dg-warning "'strnlen' specified bound 2 exceeds source size 1" "pr87492" { xfail *-*-* } }
  T (strnlen (a1, n));

  T (strnlen (a1 + 1, 0));
  T (strnlen (a1 + 1, 1));      // { dg-warning "'strnlen' specified bound 1 exceeds source size 0" }
  T (strnlen (a1 + 1, i0));
  T (strnlen (a1 + 1, i0 + 1)); // { dg-warning "'strnlen' specified bound \\\[1, \\d+] exceeds source size 0" }
  T (strnlen (a1 + 1, n));
  T (strnlen (a1 + i, 0));
  T (strnlen (a1 + i, 1));
  T (strnlen (a1 + i, 2));      // { dg-warning "'strnlen' specified bound 2 exceeds source size 1" }
  T (strnlen (a1 + i, n));
  T (strnlen (a1 + i + 1, 0));
  T (strnlen (a1 + i + 1, 1));
  T (strnlen (a1 + i + 1, 2));  // { dg-warning "'strnlen' specified bound 2 exceeds source size 1" }
  T (strnlen (a1 + i + 1, n));

  T (strnlen (a1 + i0, 0));
  T (strnlen (a1 + i0, 1));
  T (strnlen (a1 + i0, 2));     // { dg-warning "'strnlen' specified bound 2 exceeds source size 1" }
  T (strnlen (a1 + i0, n));
  T (strnlen (a1 + i0 + 1, 0));
  T (strnlen (a1 + i0 + 1, 1)); // { dg-warning "'strnlen' specified bound 1 exceeds source size 0" }
  T (strnlen (a1 + i0 + 1, n));

  T (strnlen (a2, 0));
  T (strnlen (a2, 1));
  T (strnlen (a2, 2));
  T (strnlen (a2, n));
  T (strnlen (a2 + 1, 0));
  T (strnlen (a2 + 1, 1));
  T (strnlen (a2 + 1, 2));      // { dg-warning "'strnlen' specified bound 2 exceeds source size 1"  "pr87492" }
  T (strnlen (a2 + 1, n));
  T (strnlen (a2 + 2, 0));
  T (strnlen (a2 + 2, 1));      // { dg-warning "'strnlen' specified bound 1 exceeds source size 0" }
  T (strnlen (a2 + 2, n));
  T (strnlen (a2 + i, 0));
  T (strnlen (a2 + i, 1));
  T (strnlen (a2 + i, 2));
  T (strnlen (a2 + i + 2, 0));
  T (strnlen (a2 + i + 2, 1));
  T (strnlen (a2 + i + 2, 2));
  T (strnlen (a2 + i + 2, n));

  T (strnlen (a2 + i0, 0));
  T (strnlen (a2 + i0, 1));
  T (strnlen (a2 + i0, 2));
  T (strnlen (a2 + i0, 3));     // { dg-warning "'strnlen' specified bound 3 exceeds source size 2" }
  T (strnlen (a2 + i0, n));

  T (strnlen (a2 + i0 + 1, 0));
  T (strnlen (a2 + i0 + 1, 1));
  T (strnlen (a2 + i0 + 1, 2)); // { dg-warning "'strnlen' specified bound 2 exceeds source size 1" }
  T (strnlen (a2 + i0 + 1, n));

  T (strnlen (a2 + i0 + 2, 0));
  T (strnlen (a2 + i0 + 2, 1)); // { dg-warning "'strnlen' specified bound 1 exceeds source size 0" }
  T (strnlen (a2 + i0 + 2, i0));
  T (strnlen (a2 + i0 + 2, i0 + 1)); // { dg-warning "'strnlen' specified bound \\\[1, \\d+] exceeds source size 0" }
  T (strnlen (a2 + i0 + 2, n));
}


void test_strcmp_array (const char *s, int i)
{
  T (strcmp (a0, ""));        // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-* } }

  T (strcmp (a0, s));         // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" }
  T (strcmp (a0 + i, s));     // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" }

  T (strcmp (a1, s));
  T (strcmp (a1 + 1, s));     // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" }
  T (strcmp (a1 + i, s));
  T (strcmp (a1 + i + 1, s));


  // Repeat the above with the arguments reversed.

  T (strcmp ("", a0));         // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-*} }

  T (strcmp (s, a0));         // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" }
  T (strcmp (s, a0 + i));     // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" }

  T (strcmp (s, a1));
  T (strcmp (s, a1 + 1));     // { dg-warning "'strcmp' reading 1 or more bytes from a region of size 0" }
  T (strcmp (s, a1 + i));
  T (strcmp (s, a1 + i + 1));
}

/* The number of characters read is considered to be bounded not just
   by the third argument to strncmp but also by the length of the shorter
   of the two strings.  When the string length is unknowm, verify that
   a warning is only issued for certain reading past the end but not
   otherwise.  */

void test_strncmp_array (const char *s, int i)
{
  T (strncmp (a0, a0, 0));

  T (strncmp (a0, s, 0));
  T (strncmp (a0, s, 1));       // { dg-warning "'strncmp' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-* } }

  T (strncmp (a0, s, 2));       // { dg-warning "'strncmp' (reading between 1 and 2 bytes from a region of size 0|specified bound 2 exceeds source size 0)" }
  T (strncmp (a1, s, 0));
  T (strncmp (a1, s, 1));
  T (strncmp (a1 + 1, s, 1));   // { dg-warning "'strncmp' reading 1 byte from a region of size 0" "pr?????" { xfail *-*-*} }
  T (strncmp (a1, s, 1));
  T (strncmp (a1 + 1, s, 2));   // { dg-warning "'strncmp' (reading between 1 and 2 bytes from a region of size 0|specified bound 2 exceeds source size 0)" }

  T (strncmp (a2, s, 1));
  T (strncmp (a2, s, 2));
  T (strncmp (a2, s, 3));

  T (strncmp (a2 + 1, s, 1));
  T (strncmp (a2 + 2, s, 2));   // { dg-warning "'strncmp' (reading between 1 and 2 bytes from a region of size 0|specified bound 2 exceeds source size 0)" }

  T (strncmp (a1, b1, 0));
  T (strncmp (a1, b1, 1));
  T (strncmp (a1, b1, 2));      // { dg-warning "'strncmp' specified bound 2 exceeds source size 1" }
}


void test_strncmp_literal (const char *s, int i)
{
  T (strncmp (a0, "", 0));
  T (strncmp (a0, "1", 0));
  T (strncmp (a0, "12", 0));

  /* The calls with a bound in excess of the length of the literal are
     folded early (most into strcmp) so the warning doesn't trigger.  */
  T (strncmp (s, "", 0));

  T (strncmp (s, "1", 0));
  T (strncmp (s, "1", 1));
  T (strncmp (s, "1", 2));      // { dg-warning "\\\[-Wstringop-overread" "pr93665" { xfail *-*-* } }

  T (strncmp (s, "12", 0));
  T (strncmp (s, "12", 1));
  T (strncmp (s, "12", 2));
  T (strncmp (s, "12", 3));     // { dg-warning "\\\[-Wstringop-overread" "pr93665" { xfail *-*-* } }

  T (strncmp (s, "123", 0));
  T (strncmp (s, "123", 1));
  T (strncmp (s, "123", 2));
  T (strncmp (s, "123", 3));
  T (strncmp (s, "123", 4));    // { dg-warning "\\\[-Wstringop-overread" "pr93665" { xfail *-*-* } }
}


void test_strchr_array (int x, int i)
{
  T (strchr (a0, x));         // { dg-warning "'strchr' reading 1 or more bytes from a region of size 0" }
  T (strchr (a0 + i, x));     // { dg-warning "'strchr' reading 1 or more bytes from a region of size 0" }

  T (strchr (a1, x));
  T (strchr (a1 + 1, x));     // { dg-warning "'strchr' reading 1 or more bytes from a region of size 0" }
  T (strchr (a1 + i, x));
  T (strchr (a1 + i + 1, x));
}


void test_strdup_array (int i)
{
  T (strdup (a0));            // { dg-warning "'strdup' reading 1 or more bytes from a region of size 0" }
  T (strdup (a0 + i));        // { dg-warning "'strdup' reading 1 or more bytes from a region of size 0" }

  T (strdup (a1));
  T (strdup (a1 + 1));        // { dg-warning "'strdup' reading 1 or more bytes from a region of size 0" }
  T (strdup (a1 + i));
  T (strdup (a1 + i + 1));
}


void test_strndup_array (int i, int i0, unsigned n)
{
  if (i0 < 0)
    i0 = 0;

  T (strndup (a0, 0));
  T (strndup (a0, 1));          // { dg-warning "'strndup' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }
  T (strndup (a0, i0));
  T (strndup (a0, i0 + 1));     // { dg-warning "'strndup' (reading between 1 and \[0-9\]+ bytes from a region of size 0|specified bound \\\[1, \[0-9\]+\\\] exceeds source size 0)" }
  T (strndup (a0, n));
  T (strndup (a0 + i, 0));
  T (strndup (a0 + i, 1));      // { dg-warning "'strndup' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }
  T (strndup (a0 + i, i0));
  T (strndup (a0 + i, n));
  T (strndup (a0 + i + 1, 0));
  T (strndup (a0 + i + 1, 1));  // { dg-warning "'strndup' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }

  T (strndup (a0 + i0, 0));
  T (strndup (a0 + i0, 1));     // { dg-warning "'strndup' (reading 1 byte from a region of size 0|specified bound 1 exceeds source size 0)" }
  T (strndup (a0 + i0, n));

  T (strndup (a1, 0));
  T (strndup (a1, 1));
  T (strndup (a1, 2));          // { dg-warning "'strndup' specified bound 2 exceeds source size 1" }
  T (strndup (a1, n));
  T (strndup (a1 + 1, 0));
  T (strndup (a1 + 1, 1));      // { dg-warning "'strndup' specified bound 1 exceeds source size 0" }
  T (strndup (a1 + 1, i0));
  T (strndup (a1 + 1, i0 + 1)); // { dg-warning "'strndup' specified bound \\\[1, \\d+] exceeds source size 0" }
  T (strndup (a1 + 1, n));
  T (strndup (a1 + i, 0));
  T (strndup (a1 + i, 1));
  T (strndup (a1 + i, 2));      // { dg-warning "'strndup' specified bound 2 exceeds source size 1" }
  T (strndup (a1 + i, n));
  T (strndup (a1 + i + 1, 0));
  T (strndup (a1 + i + 1, 1));
  T (strndup (a1 + i + 1, 2));  // { dg-warning "'strndup' specified bound 2 exceeds source size 1" }
  T (strndup (a1 + i + 1, n));

  T (strndup (a1 + i0, 0));
  T (strndup (a1 + i0, 1));
  T (strndup (a1 + i0, n));
  T (strndup (a1 + i0 + 1, 0));
  T (strndup (a1 + i0 + 1, 1)); // { dg-warning "'strndup' specified bound 1 exceeds source size 0" }
  T (strndup (a1 + i0 + 1, n));

  T (strndup (a2, 0));
  T (strndup (a2, 1));
  T (strndup (a2, 2));
  T (strndup (a2, n));
  T (strndup (a2 + 1, 0));
  T (strndup (a2 + 1, 1));
  T (strndup (a2 + 1, 2));      // { dg-warning "'strndup' specified bound 2 exceeds source size 1" }
  T (strndup (a2 + 1, n));
  T (strndup (a2 + 2, 0));
  T (strndup (a2 + 2, 1));      // { dg-warning "'strndup' specified bound 1 exceeds source size 0" }
  T (strndup (a2 + 2, n));
  T (strndup (a2 + i, 0));
  T (strndup (a2 + i, 1));
  T (strndup (a2 + i, 2));
  T (strndup (a2 + i + 2, 0));
  T (strndup (a2 + i + 2, 1));
  T (strndup (a2 + i + 2, 2));
  T (strndup (a2 + i + 2, n));

  T (strndup (a2 + i0, 0));
  T (strndup (a2 + i0, 1));
  T (strndup (a2 + i0, 2));
  T (strndup (a2 + i0, 3));     // { dg-warning "'strndup' specified bound 3 exceeds source size 2" }
  T (strndup (a2 + i0, n));

  T (strndup (a2 + i0 + 1, 0));
  T (strndup (a2 + i0 + 1, 1));
  T (strndup (a2 + i0 + 1, 2)); // { dg-warning "'strndup' specified bound 2 exceeds source size 1" }
  T (strndup (a2 + i0 + 1, n));

  T (strndup (a2 + i0 + 2, 0));
  T (strndup (a2 + i0 + 2, 1)); // { dg-warning "'strndup' specified bound 1 exceeds source size 0" }
  T (strndup (a2 + i0 + 2, i0));
  T (strndup (a2 + i0 + 2, i0 + 1)); // { dg-warning "'strndup' specified bound \\\[1, \\d+] exceeds source size 0" }
  T (strndup (a2 + i0 + 2, n));
}


void test_strpbrk_array (char *s, int i)
{
  T (strpbrk (a0, ""));       // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-* } }

  T (strpbrk (a0, s));        // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }
  T (strpbrk (a0 + i, s));    // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }

  T (strpbrk (a1, s));
  T (strpbrk (a1 + 1, s));    // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }
  T (strpbrk (a1 + i, s));
  T (strpbrk (a1 + i + 1, s));


  // Repeat the above with the arguments reversed.

  T (strpbrk ("", a0));       // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }

  T (strpbrk (s, a0));        // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }
  T (strpbrk (s, a0 + i));    // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }

  T (strpbrk (s, a1));
  T (strpbrk (s, a1 + 1));    // { dg-warning "'strpbrk' reading 1 or more bytes from a region of size 0" }
  T (strpbrk (s, a1 + i));
  T (strpbrk (s, a1 + i + 1));
}


void test_strspn_array (const char *s, int i)
{
  T (strspn (a0, ""));        // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-* } }

  T (strspn (a0, s));         // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" }
  T (strspn (a0 + i, s));     // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" }

  T (strspn (a1, s));
  T (strspn (a1 + 1, s));     // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" }
  T (strspn (a1 + i, s));
  T (strspn (a1 + i + 1, s));


  // Repeat the above with the arguments reversed.

  T (strspn ("", a0));         // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-*} }

  T (strspn (s, a0));         // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" }
  T (strspn (s, a0 + i));     // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" }

  T (strspn (s, a1));
  T (strspn (s, a1 + 1));     // { dg-warning "'strspn' reading 1 or more bytes from a region of size 0" }
  T (strspn (s, a1 + i));
  T (strspn (s, a1 + i + 1));
}


void test_strcspn_array (const char *s, int i)
{
  /* The call below is tranformed to strlen() so the warning references
     the latter function instead of strcspn.  Avoid testing that aspect.  */
  T (strcspn (a0, ""));       // { dg-warning "reading 1 or more bytes from a region of size 0" }

  T (strcspn (a0, s));        // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" }
  T (strcspn (a0 + i, s));    // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" }

  T (strcspn (a1, s));
  T (strcspn (a1 + 1, s));    // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" }
  T (strcspn (a1 + i, s));
  T (strcspn (a1 + i + 1, s));


  // Repeat the above with the arguments reversed.

  T (strcspn ("", a0));       // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-*} }

  T (strcspn (s, a0));        // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" }
  T (strcspn (s, a0 + i));    // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" }

  T (strcspn (s, a1));
  T (strcspn (s, a1 + 1));    // { dg-warning "'strcspn' reading 1 or more bytes from a region of size 0" }
  T (strcspn (s, a1 + i));
  T (strcspn (s, a1 + i + 1));
}


void test_strstr_array (char *s, int i)
{
  T (strstr (a0, ""));        // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" "pr?????" { xfail *-*-* } }

  T (strstr (a0, s));         // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }
  T (strstr (a0 + i, s));     // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }

  T (strstr (a1, s));
  T (strstr (a1 + 1, s));     // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }
  T (strstr (a1 + i, s));
  T (strstr (a1 + i + 1, s));


  // Repeat the above with the arguments reversed.

  T (strstr ("", a0));        // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }

  T (strstr (s, a0));         // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }
  T (strstr (s, a0 + i));     // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }

  T (strstr (s, a1));
  T (strstr (s, a1 + 1));     // { dg-warning "'strstr' reading 1 or more bytes from a region of size 0" }
  T (strstr (s, a1 + i));
  T (strstr (s, a1 + i + 1));
}


void test_puts_array (int i)
{
  T (puts (a0));              // { dg-warning "'puts' reading 1 or more bytes from a region of size 0" }
  T (puts (a0 + i));          // { dg-warning "'puts' reading 1 or more bytes from a region of size 0" }

  T (puts (a1));
  T (puts (a1 + 1));          // { dg-warning "'puts' reading 1 or more bytes from a region of size 0" }
  T (puts (a1 + i));
  T (puts (a1 + i + 1));
}


void test_puts_unlocked_array (int i)
{
  T (puts_unlocked (a0));     // { dg-warning "'puts_unlocked' reading 1 or more bytes from a region of size 0" }
  T (puts_unlocked (a0 + i)); // { dg-warning "'puts_unlocked' reading 1 or more bytes from a region of size 0" }

  T (puts_unlocked (a1));
  T (puts_unlocked (a1 + 1)); // { dg-warning "'puts_unlocked' reading 1 or more bytes from a region of size 0" }
  T (puts_unlocked (a1 + i));
  T (puts_unlocked (a1 + i + 1));
}


void test_gettext_array (int i)
{
  T (gettext (a0));           // { dg-warning "'gettext' reading 1 or more bytes from a region of size 0" }
  T (gettext (a0 + i));       // { dg-warning "'gettext' reading 1 or more bytes from a region of size 0" }

  T (gettext (a1));
  T (gettext (a1 + 1));       // { dg-warning "'gettext' reading 1 or more bytes from a region of size 0" }
  T (gettext (a1 + i));
  T (gettext (a1 + i + 1));
}
