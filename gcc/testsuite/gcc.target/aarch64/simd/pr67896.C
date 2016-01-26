typedef __Poly8_t A;
typedef __Poly16_t A; /* { dg-error "conflicting declaration" } */
typedef __Poly64_t A; /* { dg-error "conflicting declaration" } */
typedef __Poly128_t A; /* { dg-error "conflicting declaration" } */

typedef __Poly8x8_t B;
typedef __Poly16x8_t B; /* { dg-error "conflicting declaration" } */ 
