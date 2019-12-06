/* { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-truncation -ftrack-macro-expansion=0" } */

#define NULL (void*)0

const char a[] = { 'a', 'b', 'c', 'd' };
const char b[] = { 'a', '\0', 'c', '\0', 'e' };

#define CONCAT(a, b) a ## b
#define CAT(a, b)    CONCAT (a, b)

typedef struct FILE FILE;
extern FILE *fp;

extern char *d;
extern const char *s;
extern int n;

#define T(func, ...)						\
  __attribute__ ((noipa)) void					\
  CAT (test_ ## func, __LINE__) (void)				\
  {								\
    sink (0, __builtin_ ## func (__VA_ARGS__), d, s, n);	\
  } typedef void dummy_type

void sink (void*, ...);


// Exercise string functions.
T (index, a, 'x');          // { dg-warning "missing terminating nul" "index" }
T (index, a, *s);           // { dg-warning "missing terminating nul" "index" }

T (index, b, '0');
T (index, b + 1, '1');
T (index, b + 2, '2');
T (index, b + 3, '3');
T (index, b + 4, '4');      // { dg-warning "missing terminating nul" "index" }

T (rindex, a, 'x');         // { dg-warning "missing terminating nul" "rindex" }
T (rindex, a, *s);          // { dg-warning "missing terminating nul" "rindex" }

T (rindex, b, '0');
T (rindex, b + 1, '1');
T (rindex, b + 2, '2');
T (rindex, b + 3, '3');
T (rindex, b + 4, '4');     // { dg-warning "missing terminating nul" "rindex" }

T (stpcpy, d, a);           // { dg-warning "missing terminating nul" "stpcpy" }

T (stpncpy, d, a, 4);
T (stpncpy, d, a, 5);       // { dg-warning "missing terminating nul" "stpncpy" }
T (stpncpy, d, a, n);

T (stpncpy, d, a + n, 4);
T (stpncpy, d, a + n, 5);   // { dg-warning "missing terminating nul" "stpncpy" }

T (stpncpy, d, b, 4);
T (stpncpy, d, b, 5);
T (stpncpy, d, b, n);

T (stpncpy, d, b + 1, 4);
T (stpncpy, d, b + 1, 5);
T (stpncpy, d, b + 1, n);

T (stpncpy, d, b + 3, 4);
T (stpncpy, d, b + 3, 5);
T (stpncpy, d, b + 3, n);

T (stpncpy, d, b + 4, 1);
T (stpncpy, d, b + 4, 2);   // { dg-warning "missing terminating nul" "stpncpy" }
T (stpncpy, d, b + 4, n);
/* The following might be worth warning about since it's only safe with
   n < 4.  */
T (stpncpy, d, b + n, 5);

T (strcasecmp, a, "ab");    // { dg-warning "missing terminating nul" "strcasecmp" }
T (strcasecmp, a, s);       // { dg-warning "missing terminating nul" "strcasecmp" }
T (strcasecmp, a, b);       // { dg-warning "missing terminating nul" "strcasecmp" }
T (strcasecmp, b, b + 1);
T (strcasecmp, b, b + 2);
T (strcasecmp, b, b + 3);
T (strcasecmp, b, b + 4);   // { dg-warning "missing terminating nul" "strcasecmp" }

T (strcat, d, a);           // { dg-warning "missing terminating nul" "strcat" }

T (strncat, d, a, 4);
T (strncat, d, a, 5);       // { dg-warning "missing terminating nul" "strncat" }
T (strncat, d, a, n);

T (strncat, d, b, n);
T (strncat, d, b + 1, n);
T (strncat, d, b + 2, n);
T (strncat, d, b + 3, n);
T (strncat, d, b + 4, 0);
T (strncat, d, b + 4, 1);
T (strncat, d, b + 4, 2);   // { dg-warning "missing terminating nul" "strncat" }
/* The following should probably trigger a warning since it's only safe
   when n < 2, makes little sense with n == 0, and not much more with
   n == 1.  */
T (strncat, d, b + 4, n);   // { dg-warning "missing terminating nul" "strncat" { xfail *-*-* } }

T (strchr, a, 'x');         // { dg-warning "missing terminating nul" "strchr" }
T (strchr, a, *s);          // { dg-warning "missing terminating nul" "strchr" }

T (strcmp, a, "ab");        // { dg-warning "missing terminating nul" "strcmp" }
T (strcmp, "bc", a);        // { dg-warning "missing terminating nul" "strcmp" }
T (strcmp, a, s);           // { dg-warning "missing terminating nul" "strcmp" }
T (strcmp, s, a);           // { dg-warning "missing terminating nul" "strcmp" }

T (strcmp, a, b);           // { dg-warning "missing terminating nul" "strcmp" }
/* Even though most likely safe in reality because b[1] is nul,
   the following is strictly undefined because a is not a string.
   The warning is not issued because GCC folds the call to (int)*a.  */
T (strcmp, a, b + 1);       // { dg-warning "missing terminating nul" "bug" { xfail *-*-* } }

T (strncmp, a, "ab", 4);
T (strncmp, "bc", a, 4);
T (strncmp, a, a, 4);
T (strncmp, a, s, 4);
T (strncmp, s, a, 4);

/* The warning below is not issued because GCC folds strncmp calls with
   the same arguments to zero before it checks for the missing nul.  */
T (strncmp, a, a, 5);       // { dg-warning "missing terminating nul" "pr92624" { xfail *-*-*} }
T (strncmp, a, s, 5);       // { dg-warning "missing terminating nul" "strcmp" }
T (strncmp, s, a, 5);       // { dg-warning "missing terminating nul" "strcmp" }

T (strcpy, d, a);           // { dg-warning "missing terminating nul" "strcpy" }

T (strcspn, a, s);          // { dg-warning "missing terminating nul" "strcspn" }
T (strcspn, s, a);          // { dg-warning "missing terminating nul" "strcspn" }

T (strspn, a, s);           // { dg-warning "missing terminating nul" "strcspn" }
T (strspn, s, a);           // { dg-warning "missing terminating nul" "strcspn" }

T (strdup, a);              // { dg-warning "missing terminating nul" "strdup" }

T (strndup, a, 4);
T (strndup, a, 5);          // { dg-warning "missing terminating nul" "strndup" }
T (strndup, b + 3, 2);
T (strndup, b + 4, 1);
T (strndup, b + 4, 2);      // { dg-warning "missing terminating nul" "strndup" }

T (strlen, a);              // { dg-warning "missing terminating nul" "strlen" }

T (strnlen, a, 4);
T (strnlen, a, 5);          // { dg-warning "specified bound 5 exceeds the size 4 of unterminated array" "strnlen" }
T (strnlen, a, n);

T (strpbrk, s, a);          // { dg-warning "missing terminating nul" "strpbrk" }

T (strrchr, a, 'x');        // { dg-warning "missing terminating nul" "strrchr" }
T (strrchr, a, *s);         // { dg-warning "missing terminating nul" "strrchr" }

T (strstr, a, "cde");       // { dg-warning "missing terminating nul" "strstr" }
T (strstr, a, s);           // { dg-warning "missing terminating nul" "strstr" }


// Exercise a few string checking functions.
T (__stpcpy_chk, d, a, -1);           // { dg-warning "missing terminating nul" "stpcpy" }


T (__stpncpy_chk, d, a, 4, -1);
T (__stpncpy_chk, d, a, 5, -1);       // { dg-warning "missing terminating nul" "stpncpy_chk" }
T (__stpncpy_chk, d, a, n, -1);

T (__stpncpy_chk, d, a + n, 4, -1);
T (__stpncpy_chk, d, a + n, 5, -1);   // { dg-warning "missing terminating nul" "stpncpy_chk" }

T (__stpncpy_chk, d, b, 4, -1);
T (__stpncpy_chk, d, b, 5, -1);
T (__stpncpy_chk, d, b, n, -1);

T (__stpncpy_chk, d, b + 1, 4, -1);
T (__stpncpy_chk, d, b + 1, 5, -1);
T (__stpncpy_chk, d, b + 1, n, -1);

T (__stpncpy_chk, d, b + 3, 4, -1);
T (__stpncpy_chk, d, b + 3, 5, -1);
T (__stpncpy_chk, d, b + 3, n, -1);

T (__stpncpy_chk, d, b + 4, 1, -1);
T (__stpncpy_chk, d, b + 4, 2, -1);   // { dg-warning "missing terminating nul" "stpncpy_chk" }
T (__stpncpy_chk, d, b + 4, n, -1);


T (__strncat_chk, d, a, 4, -1);
T (__strncat_chk, d, a, 5, -1);       // { dg-warning "missing terminating nul" "strncat_chk" }
T (__strncat_chk, d, a, n, -1);

T (__strncat_chk, d, a + n, 4, -1);
T (__strncat_chk, d, a + n, 5, -1);   // { dg-warning "missing terminating nul" "strncat_chk" }

T (__strncat_chk, d, b, 4, -1);
T (__strncat_chk, d, b, 5, -1);
T (__strncat_chk, d, b, n, -1);

T (__strncat_chk, d, b + 1, 4, -1);
T (__strncat_chk, d, b + 1, 5, -1);
T (__strncat_chk, d, b + 1, n, -1);

T (__strncat_chk, d, b + 3, 4, -1);
T (__strncat_chk, d, b + 3, 5, -1);
T (__strncat_chk, d, b + 3, n, -1);

T (__strncat_chk, d, b + 4, 1, -1);
T (__strncat_chk, d, b + 4, 2, -1);   // { dg-warning "missing terminating nul" "strncat_chk" }
T (__strncat_chk, d, b + 4, n, -1);


T (__strncpy_chk, d, a, 4, -1);
T (__strncpy_chk, d, a, 5, -1);       // { dg-warning "missing terminating nul" "strncpy_chk" }
T (__strncpy_chk, d, a, n, -1);

T (__strncpy_chk, d, a + n, 4, -1);
T (__strncpy_chk, d, a + n, 5, -1);   // { dg-warning "missing terminating nul" "strncpy_chk" }

T (__strncpy_chk, d, b, 4, -1);
T (__strncpy_chk, d, b, 5, -1);
T (__strncpy_chk, d, b, n, -1);

T (__strncpy_chk, d, b + 1, 4, -1);
T (__strncpy_chk, d, b + 1, 5, -1);
T (__strncpy_chk, d, b + 1, n, -1);

T (__strncpy_chk, d, b + 3, 4, -1);
T (__strncpy_chk, d, b + 3, 5, -1);
T (__strncpy_chk, d, b + 3, n, -1);

T (__strncpy_chk, d, b + 4, 1, -1);
T (__strncpy_chk, d, b + 4, 2, -1);   // { dg-warning "missing terminating nul" "strncpy" }
T (__strncpy_chk, d, b + 4, n, -1);


// Exercise some stdio functions.
T (printf, a);              // { dg-warning "unterminated format string" "printf" }
T (printf, "%s", a);        // { dg-warning "not a nul-terminated string" "printf" }
T (sprintf, d, "%s", a);    // { dg-warning "not a nul-terminated string" "sprintf" }
T (snprintf, d, n, "%s", a);    // { dg-warning "not a nul-terminated string" "sprintf" }

T (__sprintf_chk, d, 0, -1, "%s", a);      // { dg-warning "not a nul-terminated string" "sprintf" }
T (__snprintf_chk, d, n, 0, -1, "%s", a);  // { dg-warning "not a nul-terminated string" "sprintf" }

T (fputs, a, fp);           // { dg-warning "missing terminating nul" "fputs" }
T (fputs_unlocked, a, fp);  // { dg-warning "missing terminating nul" "fputs_unlocked" }
T (puts, a);                // { dg-warning "missing terminating nul" "puts" }
T (puts_unlocked, a);       // { dg-warning "missing terminating nul" "puts_unlocked" }



// Exerise exec functions.
T (execl, a, s, NULL);      // { dg-warning "missing terminating nul" "execl" }
T (execl, a, s, NULL);      // { dg-warning "missing terminating nul" "execl" }
T (execle, a, s, NULL, NULL);   // { dg-warning "missing terminating nul" "execl" }
T (execlp, a, s, NULL);     // { dg-warning "missing terminating nul" "execl" }

T (execv, a, &d);           // { dg-warning "missing terminating nul" "execl" }
T (execve, a, &d, &d);      // { dg-warning "missing terminating nul" "execl" }
T (execvp, a, &d);          // { dg-warning "missing terminating nul" "execl" }

T (gettext, a);             // { dg-warning "missing terminating nul" "gettext" }

T (strfmon, d, n, a);       // { dg-warning "unterminated format string" "strfmon" }
