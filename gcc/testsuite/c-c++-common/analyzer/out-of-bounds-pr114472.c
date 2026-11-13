/* Verify we don't ICE on cases involving very large values for size.  */

char s, d;

void
foo(void)
{
  __builtin_strncpy(&d, &s + 3, -1); /* { dg-warning "Wstringop-overflow" } */
  /* { dg-warning "buffer over-read" "" { target *-*-* } .-1 } */
  /* { dg-warning "buffer overflow" "" { target *-*-* } .-2 } */
  __builtin_strncpy(&d + 3, &s, -1); /* { dg-warning "Wstringop-overflow" } */
  /* { dg-warning "buffer over-read" "" { target *-*-* } .-1 } */
  /* { dg-warning "buffer overflow" "" { target *-*-* } .-2 } */
}

void
bar(void)
{
  __builtin_strncpy(&d, &s - 3, -1); /* { dg-warning "Wstringop-overflow" } */
  /* { dg-warning "buffer under-read" "" { target *-*-* } .-1 } */
  /* { dg-warning "buffer over-read" "" { target *-*-* } .-2 } */
  /* { dg-warning "buffer overflow" "" { target *-*-* } .-3 } */
  __builtin_strncpy(&d - 3, &s, -1); /* { dg-warning "Wstringop-overflow" } */
  /* { dg-warning "buffer overflow" "" { target *-*-* } .-1 } */
  /* { dg-warning "buffer over-read" "" { target *-*-* } .-2 } */
  /* { dg-warning "buffer underwrite" "" { target *-*-* } .-3 } */
}
