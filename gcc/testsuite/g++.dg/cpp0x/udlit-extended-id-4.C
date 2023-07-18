// { dg-options "-std=c++98 -Wc++11-compat" }
#define END ;
#define εND ;
#define EηD ;
#define EN\u0394 ;

const char *s1 = "s1"END // { dg-warning "requires a space between string literal and macro" }
const char *s2 = "s2"εND // { dg-warning "requires a space between string literal and macro" }
const char *s3 = "s3"EηD // { dg-warning "requires a space between string literal and macro" }
const char *s4 = "s4"ENΔ // { dg-warning "requires a space between string literal and macro" }

/* Make sure we did not skip the token also in the case that it wasn't found to
   be a macro; compilation should fail here.  */
const char *s5 = "s5"NØT_A_MACRO; // { dg-error "expected ',' or ';' before" }
