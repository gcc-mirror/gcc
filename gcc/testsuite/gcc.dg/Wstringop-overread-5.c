/* Verify -Wstringop-overread with a source pointer pointing either
   before the beginning or past the end of an object.
   { dg-do compile }
   { dg-options "-O -Wall -Wno-array-bounds" } */

typedef __SIZE_TYPE__ size_t;

size_t strlen (const char *);

void sink (void*, ...);

void off_sz_or_1 (size_t i)
{
  i |= 1;

  /* Verify the offset in the notes only mentions the meaningful lower
     bound and not a range with the excessive (and meaningless) upper
     bound like [2, 9223372036854775807].  */
  extern char a[1];
  // { dg-message "at offset 1 into source object 'a'" "note" { target *-*-* } .-1 }
  // { dg-message "at offset 2 " "note" { target *-*-* } .-2 }

  char *p1 = a + i;
  char *p2 = p1 + 1;
  char *p3 = p1 - 1;

  size_t n = 0;
  n += strlen (p1);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n += strlen (p2);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n += strlen (p3);

  sink (p1, p2, p3, n);
}


void off_sz_or_2 (size_t i)
{
  i |= 2;

  extern char b[2];
  // { dg-message "at offset 2 " "note" { target *-*-* } .-1 }
  // { dg-message "at offset 3 " "note" { target *-*-* } .-2 }

  char *p1 = b + i;
  char *p2 = p1 + 1;
  char *p3 = p1 - 1;

  size_t n = 0;
  n += strlen (p1);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n += strlen (p2);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n += strlen (p3);

  sink (p1, p2, p3, n);
}


void off_sz_or_4 (size_t i)
{
  i |= 4;

  extern char c[3];
  // { dg-message "at offset 4 " "note" { target *-*-* } .-1 }
  // { dg-message "at offset 5 " "note" { target *-*-* } .-2 }
  // { dg-message "at offset 3 " "note" { target *-*-* } .-3 }

  char *p1 = c + i;
  char *p2 = p1 + 1;
  char *p3 = p1 - 1;

  size_t n = 0;
  n += strlen (p1);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n += strlen (p2);     // { dg-warning "reading 1 or more bytes from a region of size 0" }
  n += strlen (p3);     // { dg-warning "reading 1 or more bytes from a region of size 0" }

  sink (p1, p2, p3, n);
}
