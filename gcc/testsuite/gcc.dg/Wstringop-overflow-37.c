/* Verify that -Wstringop-overflow detects writing past the end of each
   individual element of a multidimensional array.
  { dg-do compile }
  { dg-options "-O2 -Wall -Wno-array-bounds -Wno-stringop-truncation" } */

#define CONCAT(x, y)    x ## y
#define CAT(name, line) CONCAT (name, line)
#define UNIQ_NAME(name) CAT (name, __LINE__)

typedef __SIZE_TYPE__ size_t;

extern void* malloc (size_t);
extern char* strncpy (char*, const char*, size_t);

extern char a2_2_8[2][2][8];

void nowarn_a2_2_8 (const char *s)
{
  // The following trigger -Wstringop-truncation.
  strncpy (a2_2_8[0][0], s, 8);
  strncpy (a2_2_8[0][1], s, 8);
  strncpy (a2_2_8[1][0], s, 8);
  strncpy (a2_2_8[1][1], s, 8);
}

void warn_a2_2_8 (const char *s)
{
  strncpy (a2_2_8[0][0], s, 9);         // { dg-warning "writing 9 bytes into a region of size 8 " }
  strncpy (a2_2_8[0][1], s, 9);         // { dg-warning "writing 9 bytes into a region of size 8 " }
  strncpy (a2_2_8[1][0], s, 9);         // { dg-warning "writing 9 bytes into a region of size 8 " }
  strncpy (a2_2_8[1][1], s, 9);         // { dg-warning "writing 9 bytes into a region of size 8 " }
}


extern char a2_3_5[2][3][5];

void nowarn_a2_3_5 (const char *s)
{
  // The following trigger -Wstringop-truncation.
  strncpy (a2_3_5[0][0], s, 5);
  strncpy (a2_3_5[0][1], s, 5);
  strncpy (a2_3_5[0][2], s, 5);
  strncpy (a2_3_5[1][0], s, 5);
  strncpy (a2_3_5[1][1], s, 5);
  strncpy (a2_3_5[1][2], s, 5);
}

void warn_a2_3_5 (const char *s)
{
  strncpy (a2_3_5[0][0], s, 6);         // { dg-warning "writing 6 bytes into a region of size 5 " }
  strncpy (a2_3_5[0][1], s, 7);         // { dg-warning "writing 7 bytes into a region of size 5 " }
  strncpy (a2_3_5[1][0], s, 8);         // { dg-warning "writing 8 bytes into a region of size 5 " }
  strncpy (a2_3_5[1][1], s, 9);         // { dg-warning "writing 9 bytes into a region of size 5 " }
}


void* nowarn_malloc_3_5 (const char *s, unsigned n)
{
  if (n < 3 || 5 < n)
    n = 3;
  char *p = (char*)malloc (n);
  strncpy (p + 1, s, 4);
  return p;
}

void* warn_malloc_3_5 (const char *s, unsigned n)
{
  if (n < 3 || 5 < n)
    n = 3;
  char *p = (char*)malloc (n);          // { dg-message "at offset 1 into destination object of size \\\[3, 5] allocated by 'malloc'" }
  // The size below should be a range like the one above.
  strncpy (p + 1, s, 5);                // { dg-warning "writing 5 bytes into a region of size 4 " }
  return p;
}


typedef __attribute__ ((alloc_size (1, 2))) void* UsrAlloc (int, int);

void* nowarn_use_alloc_3_5 (UsrAlloc *usr_alloc, const char *s, unsigned n)
{
  if (n < 3 || 5 < n)
    n = 3;
  char *p = (char*)usr_alloc (n, 3);
  strncpy (p + 1, s, 14);
  return p;
}

void* warn_usr_alloc_3_5 (UsrAlloc *usr_alloc, const char *s, unsigned n)
{
  if (n < 3 || 5 < n)
    n = 3;
  char *p = (char*)usr_alloc (n, 3);    // { dg-message "at offset 1 into destination object of size \\\[9, 15] allocated by 'usr_alloc'" }
  // The size below should be a range like the one above.
  strncpy (p + 1, s, 15);               // { dg-warning "writing 15 bytes into a region of size 14 " }
  return p;
}

struct S
{
  char a2_3_4[2][3][4];
  char a3_4_5[3][4][5];
};

extern struct S sa[];

void nowarn_sa_cstidx_cstsize (const char* const s[])
{
  strncpy (sa[0].a2_3_4[0][0], s[0], 4);
  strncpy (sa[0].a2_3_4[0][1], s[1], 4);
  strncpy (sa[0].a2_3_4[0][2], s[2], 4);

  strncpy (sa[0].a2_3_4[1][0], s[3], 4);
  strncpy (sa[0].a2_3_4[1][1], s[4], 4);
  strncpy (sa[0].a2_3_4[1][2], s[5], 4);

  strncpy (sa[1].a2_3_4[0][0], s[6], 4);
  strncpy (sa[1].a2_3_4[0][1], s[7], 4);
  strncpy (sa[1].a2_3_4[0][2], s[8], 4);

  strncpy (sa[1].a2_3_4[1][0], s[9], 4);
  strncpy (sa[1].a2_3_4[1][1], s[10], 4);
  strncpy (sa[1].a2_3_4[1][2], s[11], 4);
}

void warn_sa_cstidx_cstsize (const char* const s[])
{
  strncpy (sa[0].a2_3_4[0][0], s[0], 5);    // { dg-warning "writing 5 bytes into a region of size 4 " }
  strncpy (sa[0].a2_3_4[0][1], s[1], 6);    // { dg-warning "writing 6 bytes into a region of size 4 " }
  strncpy (sa[0].a2_3_4[0][2], s[2], 7);    // { dg-warning "writing 7 bytes into a region of size 4 " }

  strncpy (sa[0].a2_3_4[1][0], s[3], 5);    // { dg-warning "writing 5 bytes into a region of size 4 " }
  strncpy (sa[0].a2_3_4[1][1], s[4], 6);    // { dg-warning "writing 6 bytes into a region of size 4 " }
  strncpy (sa[0].a2_3_4[1][2], s[5], 7);    // { dg-warning "writing 7 bytes into a region of size 4 " }

  strncpy (sa[1].a2_3_4[0][0], s[6], 5);    // { dg-warning "writing 5 bytes into a region of size 4 " }
  strncpy (sa[1].a2_3_4[0][1], s[7], 6);    // { dg-warning "writing 6 bytes into a region of size 4 " }
  strncpy (sa[1].a2_3_4[0][2], s[8], 7);    // { dg-warning "writing 7 bytes into a region of size 4 " }

  strncpy (sa[1].a2_3_4[1][0], s[9], 5);    // { dg-warning "writing 5 bytes into a region of size 4 " }
  strncpy (sa[1].a2_3_4[1][1], s[10], 6);   // { dg-warning "writing 6 bytes into a region of size 4 " }
  strncpy (sa[1].a2_3_4[1][2], s[11], 7);   // { dg-warning "writing 7 bytes into a region of size 4 " }
}

void nowarn_sa_cstidx_varsize (const char* const s[], unsigned n)
{
  strncpy (sa[0].a2_3_4[0][0], s[0], n);
  strncpy (sa[0].a2_3_4[0][1], s[1], n);
  strncpy (sa[0].a2_3_4[0][2], s[2], n);

  strncpy (sa[0].a2_3_4[1][0], s[3], n);
  strncpy (sa[0].a2_3_4[1][1], s[4], n);
  strncpy (sa[0].a2_3_4[1][2], s[5], n);

  strncpy (sa[1].a2_3_4[0][0], s[6], n);
  strncpy (sa[1].a2_3_4[0][1], s[7], n);
  strncpy (sa[1].a2_3_4[0][2], s[8], n);

  strncpy (sa[1].a2_3_4[1][0], s[9], n);
  strncpy (sa[1].a2_3_4[1][1], s[10], n);
  strncpy (sa[1].a2_3_4[1][2], s[11], n);
}

void nowarn_sa_loop (const char* const s[], unsigned n)
{
  for (unsigned i0 = 0; i0 != 5; ++i0)
    for (unsigned i1 = 0; i1 != 3; ++i1)
      for (unsigned i2 = 0; i2 != 2; ++i2)
	strncpy (sa[i0].a2_3_4[i1][i2], s[i2], n);
}


/* Verify that a note after the warning points to the accessed object
   and mentions the starting offset of the access.  Another alternative
   might be for the offset to be the starting offset of the overflow.
   As it is, it's not clear to which of the two the offset refers.  */

void test_note (const char *s)
{
  extern void sink (void*);

  {
    char a[1][1][2];                    // { dg-message "destination object" }
    strncpy (a[0][0], s, 3);            // { dg-warning "writing 3 bytes into a region of size 2 " }
    sink (a);
  }

  {
    char a[1][2][2];                    // { dg-message "at offset 2 into " }
    strncpy (a[0][1], s, 3);            // { dg-warning "writing 3 bytes into a region of size 2 " }
    sink (a);
  }

  {
    char a[1][2][2];                    // { dg-message "at offset 4 into " }
    strncpy (a[1][0], s, 3);            // { dg-warning "writing 3 bytes into a region of size 2 " }
    sink (a);
  }

  {
    char a[2][1][2];                    // { dg-message "at offset 2 into " }
    strncpy (a[1][0], s, 3);            // { dg-warning "writing 3 bytes into a region of size 2 " }
    sink (a);
  }

  {
    char a[2][2][3];                    // { dg-message "at offset 9 into " }
    strncpy (a[1][1], s, 4);            // { dg-warning "writing 4 bytes into a region of size 3 " }
    sink (a);
  }

  {
    char a[2][3][3];                    // { dg-message "at offset 12 into " }
    strncpy (a[1][1], s, 5);            // { dg-warning "writing 5 bytes into a region of size 3 " }
    sink (a);
  }

  {
    char a[2][3][3];                    // { dg-message "at offset 12 into " }
    strncpy (a[1][1], s, 6);            // { dg-warning "writing 6 bytes into a region of size 3 " }
    sink (a);
  }

  {
    char a[2][3][3];                    // { dg-message "at offset 15 into " }
    strncpy (a[1][2], s, 7);            // { dg-warning "writing 7 bytes into a region of size 3 " }
    sink (a);
  }

}
