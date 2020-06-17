/* PR middle-end/91582 - missing heap overflow detection for strcpy
   PR middle-end/85484 - missing -Wstringop-overflow for strcpy with
   a string of non-const length
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

typedef __SIZE_TYPE__ size_t;

extern void* calloc (size_t, size_t);
extern void* malloc (size_t);
extern void* memcpy (void*, const void*, size_t);
extern void* memset (void*, int, size_t);
extern char* strcpy (char*, const char*);
extern size_t strlen (const char*);

void sink (void*);


void test_memcpy_nowarn (const void *s, int i, size_t n)
{
  sink (memcpy (calloc (1, 1), s, 1));
  sink (memcpy (calloc (1, 2), s, 1));
  sink (memcpy (calloc (2, 1), s, 1));
  sink (memcpy (calloc (3, 1), s, 2));
  sink (memcpy (calloc (3, 1), "12", 2));
  sink (memcpy (calloc (3, 1), s, 3));
  sink (memcpy (calloc (3, 1), "12", 3));
  sink (memcpy (calloc (i, 1), s, 1));
  sink (memcpy (calloc (n, 1), s, 1));
  sink (memcpy (calloc (1, n), "", 1));
  sink (memcpy (calloc (1, i), "", 1));
  sink (memcpy (calloc (i, 1), "123", 3));
  sink (memcpy (calloc (n, 1), "123", 3));
  sink (memcpy (calloc (1, i), "123456", 7));
  sink (memcpy (calloc (1, n), "123456", 7));
  sink (memcpy (calloc (n, 1), s, 12345));
  sink (memcpy (calloc (1, n), s, n - 1));
  sink (memcpy (calloc (n, 1), s, n));

  sink (memcpy ((char*)calloc (1, 1) + i, "123", 1));
  sink (memcpy ((char*)calloc (n, 1) + i, "123", n));

  sink (memcpy ((char*)calloc (1, 1) + i, s, 1));
  sink (memcpy ((char*)calloc (n, 1) + i, s, n));

  sink (memcpy (malloc (1), s, 1));
  sink (memcpy (malloc (2), s, 1));
  sink (memcpy (malloc (3), s, 2));
  sink (memcpy (malloc (3), "12", 2));
  sink (memcpy (malloc (3), s, 3));
  sink (memcpy (malloc (3), "12", 3));
  sink (memcpy (malloc (n), s, 1));
  sink (memcpy (malloc (n), "", 1));
  sink (memcpy (malloc (n), "123", 3));
  sink (memcpy (malloc (n), "123456", 7));
  sink (memcpy (malloc (n), s, 12345));
  sink (memcpy (malloc (n), s, n - 1));
  sink (memcpy (malloc (n), s, n));

  {
    const int a[] = { 1, 2, 3, 4 };
    void *p = (char*)malloc (sizeof a);
    memcpy (p, a, sizeof a);
    sink (p);
  }

  {
    const int a[] = { 1, 2, 3, 4, 5 };
    size_t nelts = sizeof a / sizeof *a;
    int vla[nelts];
    memcpy (vla, a, nelts * sizeof *vla);
    sink (vla);
  }
}


void test_memcpy_warn (const int *s, size_t n)
{
  {
    void *p = (char*)malloc (0);
    memcpy (p, s, 1);                    // { dg-warning "writing 1 byte into a region of size 0" }
    sink (p);
  }

  {
    void *p = (char*)malloc (1);
    memcpy (p, s, 2);                    // { dg-warning "writing 2 bytes into a region of size 1" }
    sink (p);
  }

  {
    void *p = (char*)malloc (2);
    memcpy (p, s, 3);                    // { dg-warning "writing 3 bytes into a region of size 2" }
    sink (p);
  }

  {
    void *p = (char*)malloc (3);
    memcpy (p, s, 4);                    // { dg-warning "writing 4 bytes into a region of size 3" }
    sink (p);
  }

  {
    const int a[] = { 1, 2, 3, 4 };
    void *p = (char*)malloc (sizeof *a);
    memcpy (p, a, sizeof a);              // { dg-warning "" }
    sink (p);
  }

  {
    const int a[] = { 1, 2, 3, 4, 5 };
    size_t nelts = sizeof a / sizeof *a;
    char vla[nelts];
    memcpy (vla, a, nelts * sizeof *a);   // { dg-warning "" }
    sink (vla);
  }

  {
    void *p = malloc (n);
    memcpy (p, s, n * sizeof *s);         // { dg-warning "\\\[-Wstringop-overflow" "" { xfail *-*-* } }
    sink (p);
  }
}

void test_memset_nowarn (int x, size_t n)
{
  sink (memset (calloc (1, 1), x, 1));
  sink (memset (calloc (1, 2), x, 1));
  sink (memset (calloc (2, 1), x, 1));
  sink (memset (calloc (3, 1), x, 2));
  sink (memset (calloc (3, 1), x, 3));
  sink (memset (calloc (n, 1), x, 1));
  sink (memset (calloc (n, 1), x, 12345));
  sink (memset (calloc (1, n), x, n - 1));
  sink (memset (calloc (n, 1), x, n));

  sink (memset (malloc (1), x, 1));
  sink (memset (malloc (2), x, 1));
  sink (memset (malloc (3), x, 2));
  sink (memset (malloc (3), x, 3));
  sink (memset (malloc (n), x, 1));
  sink (memset (malloc (n), x, 12345));
  sink (memset (malloc (n), x, n - 1));
  sink (memset (malloc (n), x, n));

  {
    const int a[] = { 1, 2, 3, 4 };
    void *p = (char*)malloc (sizeof a);
    memset (p, x, sizeof a);
    sink (p);
  }

  {
    const int a[] = { 1, 2, 3, 4, 5 };
    size_t nelts = sizeof a / sizeof *a;
    int vla[nelts];
    memset (vla, x, nelts * sizeof *vla);
    sink (vla);
  }
}


void test_memset_warn (int x, size_t n)
{
  {
    void *p = (char*)malloc (0);
    memset (p, x, 1);                    // { dg-warning "writing 1 byte into a region of size 0" }
    sink (p);
  }

  {
    void *p = (char*)malloc (1);
    memset (p, x, 2);                    // { dg-warning "writing 2 bytes into a region of size 1" }
    sink (p);
  }

  {
    void *p = (char*)malloc (2);
    memset (p, x, 3);                    // { dg-warning "writing 3 bytes into a region of size 2" }
    sink (p);
  }

  {
    void *p = (char*)malloc (3);
    memset (p, x, 4);                    // { dg-warning "writing 4 bytes into a region of size 3" }
    sink (p);
  }

  {
    const int a[] = { 1, 2, 3, 4 };
    void *p = (char*)malloc (sizeof *a);
    memset (p, 0, sizeof a);              // { dg-warning "" }
    sink (p);
  }

  {
    const int a[] = { 1, 2, 3, 4, 5 };
    size_t nelts = sizeof a / sizeof *a;
    char vla[nelts];
    memset (vla, 0, nelts * sizeof *a);   // { dg-warning "" }
    sink (vla);
  }

  {
    void *p = malloc (n);
    memset (p, x, n * sizeof (int));      // { dg-warning "\\\[-Wstringop-overflow" "" { xfail *-*-* } }
    sink (p);
  }
}


void test_strcpy_nowarn (const char *s)
{
  {
    const char a[] = "12";
    int n = strlen (a);
    char *t = (char*)calloc (2, n);
    strcpy (t, a);
    sink (t);
  }

  {
    const char a[] = "123";
    unsigned n = strlen (a) + 1;
    char *t = (char*)calloc (n, 1);
    strcpy (t, a);
    sink (t);
  }

  {
    const char a[] = "1234";
    size_t n = strlen (a) * 2;
    char *t = (char*)malloc (n);
    strcpy (t, a);
    sink (t);
  }

  {
    const char a[] = "1234";
    size_t len = strlen (a) + 1;
    char vla[len];
    strcpy (vla, a);
    sink (vla);
  }

  {
    size_t n = strlen (s) + 1;
    char *t = (char*)malloc (n);
    strcpy (t, s);
    sink (t);
  }
}


void test_strcpy_warn (const char *s)
{
  {
    const char a[] = "123";
    /* Verify that using signed int for the strlen result works (i.e.,
       that the conversion from signed int to size_t doesn't prevent
       the detection.  */
    int n = strlen (a);
    char *t = (char*)calloc (n, 1);     // { dg-message "at offset 0 to an object with size 3 allocated by 'calloc' here" "calloc note 1" { xfail *-*-* } }
                                        // { dg-message "at offset 0 to an object with size at most 3 allocated by 'calloc' here" "calloc note 2" { target *-*-* } .-1 }
    strcpy (t, a);                      // { dg-warning "writing 4 bytes into a region of size (between 0 and )?3 " }

    sink (t);
  }

  {
    const char a[] = "1234";
    size_t n = strlen (a);
    char *t = (char*)malloc (n);        // { dg-message "at offset 0 to an object with size 4 allocated by 'malloc' here" "malloc note 1" { xfail *-*-* } }
                                        // { dg-message "at offset 0 to an object with size at most 4 allocated by 'malloc' here" "malloc note 2" { target *-*-* } .-1 }
    strcpy (t, a);                      // { dg-warning "writing 5 bytes into a region of size (between 0 and )?4 " }
    sink (t);
  }

  // Exercise PR middle-end/85484.
  {
    size_t len = strlen (s);
    char vla[len];                      // { dg-message "at offset 0 to an object declared here" "vla note" }
    strcpy (vla, s);                    // { dg-warning "writing one too many bytes into a region of a size that depends on 'strlen'" }
    sink (vla);
  }

  {
    size_t n = strlen (s);
    char *t = (char*)malloc (n);        // { dg-message "at offset 0 to an object allocated by 'malloc' here" "malloc note" }
    strcpy (t, s);                      // { dg-warning "writing one too many bytes into a region of a size that depends on 'strlen'" }
    sink (t);
  }
}
