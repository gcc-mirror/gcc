/* PR target/98218 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2" } */

typedef char vec __attribute__((vector_size(8)));

vec lt (vec a, vec b) { return a < b; }
vec le (vec a, vec b) { return a <= b; }
vec eq (vec a, vec b) { return a == b; }
vec ne (vec a, vec b) { return a != b; }
vec ge (vec a, vec b) { return a >= b; }
vec gt (vec a, vec b) { return a > b; }

typedef unsigned char uvec __attribute__((vector_size(8)));

vec ltu (uvec a, uvec b) { return a < b; }
vec leu (uvec a, uvec b) { return a <= b; }
vec geu (uvec a, uvec b) { return a >= b; }
vec gtu (uvec a, uvec b) { return a > b; }

/* { dg-final { scan-assembler-not "cmpb" } } */
