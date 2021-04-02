/* { dg-options "-msve-vector-bits=1024 -O3" } */

typedef __SVInt8_t vec __attribute__((arm_sve_vector_bits(1024)));
struct pair { vec v[2]; };
void use (struct pair *);
vec f (struct pair p) { vec v = p.v[1]; use (&p); return v; }
