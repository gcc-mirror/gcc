/* { dg-do compile } */

__attribute__((__simd__("bug")))
int simd_attr (void) { return 0; } /* { dg-error "only 'inbranch' and 'notinbranch'" } */

__attribute__((__simd__("notinbranch", "inbranch")))
int simd_attr2 (void) { return 0; } /* { dg-error "wrong number of arguments specified" } */
