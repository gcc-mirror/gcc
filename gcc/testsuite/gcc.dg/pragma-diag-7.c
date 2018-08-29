/* { dg-do compile } */

unsigned long ok = 0UL;
#pragma GCC diagnostic push
#pragma GCC diagnostic warning "-Wtraditional"
unsigned long bad = 1UL; /* { dg-warning "suffix" } */
/* Note the extra space before the pragma on this next line: */
 #pragma GCC diagnostic pop
unsigned long ok_again = 2UL; /* { dg-bogus "suffix" } */

/* Redundant with the previous pop, but just shows that it fails to stop the
 * following warning with an unpatched GCC: */
#pragma GCC diagnostic ignored "-Wtraditional"

/* { dg-bogus "would be stringified" "" { target *-*-* } .+1 } */
#define UNW_DEC_PROLOGUE(fmt, body, rlen, arg) \
  do {									\
      unw_rlen = rlen;							\
      *(int *)arg = body;						\
      printf("    %s:%s(rlen=%lu)\n",					\
             fmt, (body ? "body" : "prologue"), (unsigned long)rlen);	\
  } while (0)
