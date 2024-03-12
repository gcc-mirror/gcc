/* Test for redefining macros with mismatch token count (and the oddity). */

/* { dg-do preprocess } */
/* { dg-options "-DC -DD=1 -DE" } */

#define A
#define A 1
#define B 2 3
#define B 2
#define C 1
#define D 1 2
#define E

/* { dg-warning "redefined" "redef A"      { target *-*-* } 7  }
   { dg-warning "redefined" "redef B"      { target *-*-* } 9  }
   { dg-warning "redefined" "redef D"      { target *-*-* } 11 }
   { dg-warning "redefined" "redef E"      { target *-*-* } 12 }
   { dg-message "previous"  "prev def A"   { target *-*-* } 6  }
   { dg-message "previous"  "prev def B"   { target *-*-* } 8  }
   { dg-message "previous"  "prev def D/E" { target *-*-* } 0  }
*/
