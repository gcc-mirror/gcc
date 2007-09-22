/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.3 Unary ~.  */

void test ()
{
  short _Fract sf;
  _Fract f;
  long _Fract lf;
  long long _Fract llf;
  unsigned short _Fract usf;
  unsigned _Fract uf;
  unsigned long _Fract ulf;
  unsigned long long _Fract ullf;
  _Sat short _Fract Ssf;
  _Sat _Fract Sf;
  _Sat long _Fract Slf;
  _Sat long long _Fract Sllf;
  _Sat unsigned short _Fract Susf;
  _Sat unsigned _Fract Suf;
  _Sat unsigned long _Fract Sulf;
  _Sat unsigned long long _Fract Sullf;
  short _Accum sa;
  _Accum a;
  long _Accum la;
  long long _Accum lla;
  unsigned short _Accum usa;
  unsigned _Accum ua;
  unsigned long _Accum ula;
  unsigned long long _Accum ulla;
  _Sat short _Accum Ssa;
  _Sat _Accum Sa;
  _Sat long _Accum Sla;
  _Sat long long _Accum Slla;
  _Sat unsigned short _Accum Susa;
  _Sat unsigned _Accum Sua;
  _Sat unsigned long _Accum Sula;
  _Sat unsigned long long _Accum Sulla;

  /* C99 Section 6.5.3.3 ~ shall have integer types.  */

  ~sf; /* { dg-error "wrong type argument to bit-complement" } */
  ~f; /* { dg-error "wrong type argument to bit-complement" } */
  ~lf; /* { dg-error "wrong type argument to bit-complement" } */
  ~llf; /* { dg-error "wrong type argument to bit-complement" } */
  ~usf; /* { dg-error "wrong type argument to bit-complement" } */
  ~uf; /* { dg-error "wrong type argument to bit-complement" } */
  ~ulf; /* { dg-error "wrong type argument to bit-complement" } */
  ~ullf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Ssf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Slf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sllf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Susf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Suf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sulf; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sullf; /* { dg-error "wrong type argument to bit-complement" } */
  ~sa; /* { dg-error "wrong type argument to bit-complement" } */
  ~a; /* { dg-error "wrong type argument to bit-complement" } */
  ~la; /* { dg-error "wrong type argument to bit-complement" } */
  ~lla; /* { dg-error "wrong type argument to bit-complement" } */
  ~usa; /* { dg-error "wrong type argument to bit-complement" } */
  ~ua; /* { dg-error "wrong type argument to bit-complement" } */
  ~ula; /* { dg-error "wrong type argument to bit-complement" } */
  ~ulla; /* { dg-error "wrong type argument to bit-complement" } */
  ~Ssa; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sa; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sla; /* { dg-error "wrong type argument to bit-complement" } */
  ~Slla; /* { dg-error "wrong type argument to bit-complement" } */
  ~Susa; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sua; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sula; /* { dg-error "wrong type argument to bit-complement" } */
  ~Sulla; /* { dg-error "wrong type argument to bit-complement" } */

}
