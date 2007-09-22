/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.4 Cast operators.

   Test invalid casts involving fixed-point.  */

#define CAST(NAME,TYPE) \
  struct s ## NAME { TYPE NAME; } as ## NAME; \
  union u ## NAME { TYPE NAME; } au ## NAME; \
  TYPE NAME; \
  TYPE * NAME ## p;

CAST (sf, short _Fract)
CAST (f, _Fract)
CAST (lf, long _Fract)
CAST (llf, long long _Fract)
CAST (usf, unsigned short _Fract)
CAST (uf, unsigned _Fract)
CAST (ulf, unsigned long _Fract)
CAST (ullf, unsigned long long _Fract)
CAST (Ssf, _Sat short _Fract)
CAST (Sf, _Sat _Fract)
CAST (Slf, _Sat long _Fract)
CAST (Sllf, _Sat long long _Fract)
CAST (Susf, _Sat unsigned short _Fract)
CAST (Suf, _Sat unsigned _Fract)
CAST (Sulf, _Sat unsigned long _Fract)
CAST (Sullf, _Sat unsigned long long _Fract)
CAST (sa, short _Accum)
CAST (a, _Accum)
CAST (la, long _Accum)
CAST (lla, long long _Accum)
CAST (usa, unsigned short _Accum)
CAST (ua, unsigned _Accum)
CAST (ula, unsigned long _Accum)
CAST (ulla, unsigned long long _Accum)
CAST (Ssa, _Sat short _Accum)
CAST (Sa, _Sat _Accum)
CAST (Sla, _Sat long _Accum)
CAST (Slla, _Sat long long _Accum)
CAST (Susa, _Sat unsigned short _Accum)
CAST (Sua, _Sat unsigned _Accum)
CAST (Sula, _Sat unsigned long _Accum)
CAST (Sulla, _Sat unsigned long long _Accum)

void
test (void)
{
  (short _Fract []) sfp; /* { dg-error "cast specifies array type" } */
  (_Fract []) fp; /* { dg-error "cast specifies array type" } */
  (long _Fract []) lfp; /* { dg-error "cast specifies array type" } */
  (long long _Fract []) llfp; /* { dg-error "cast specifies array type" } */
  (unsigned short _Fract []) usfp; /* { dg-error "cast specifies array type" } */
  (unsigned _Fract []) ufp; /* { dg-error "cast specifies array type" } */
  (unsigned long _Fract []) ulfp; /* { dg-error "cast specifies array type" } */
  (unsigned long long _Fract []) ullfp; /* { dg-error "cast specifies array type" } */
  (_Sat short _Fract []) Ssfp; /* { dg-error "cast specifies array type" } */
  (_Sat _Fract []) Sfp; /* { dg-error "cast specifies array type" } */
  (_Sat long _Fract []) Slfp; /* { dg-error "cast specifies array type" } */
  (_Sat long long _Fract []) Sllfp; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned short _Fract []) Susfp; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned _Fract []) Sufp; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned long _Fract []) Sulfp; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned long long _Fract []) Sullfp; /* { dg-error "cast specifies array type" } */
  (short _Accum []) sap; /* { dg-error "cast specifies array type" } */
  (_Accum []) ap; /* { dg-error "cast specifies array type" } */
  (long _Accum []) lap; /* { dg-error "cast specifies array type" } */
  (long long _Accum []) llap; /* { dg-error "cast specifies array type" } */
  (unsigned short _Accum []) usap; /* { dg-error "cast specifies array type" } */
  (unsigned _Accum []) uap; /* { dg-error "cast specifies array type" } */
  (unsigned long _Accum []) ulap; /* { dg-error "cast specifies array type" } */
  (unsigned long long _Accum []) ullap; /* { dg-error "cast specifies array type" } */
  (_Sat short _Accum []) Ssap; /* { dg-error "cast specifies array type" } */
  (_Sat _Accum []) Sap; /* { dg-error "cast specifies array type" } */
  (_Sat long _Accum []) Slap; /* { dg-error "cast specifies array type" } */
  (_Sat long long _Accum []) Sllap; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned short _Accum []) Susap; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned _Accum []) Suap; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned long _Accum []) Sulap; /* { dg-error "cast specifies array type" } */
  (_Sat unsigned long long _Accum []) Sullap; /* { dg-error "cast specifies array type" } */

  (short _Fract ()) sfp; /* { dg-error "cast specifies function type" } */
  (_Fract ()) fp; /* { dg-error "cast specifies function type" } */
  (long _Fract ()) lfp; /* { dg-error "cast specifies function type" } */
  (long long _Fract ()) llfp; /* { dg-error "cast specifies function type" } */
  (unsigned short _Fract ()) usfp; /* { dg-error "cast specifies function type" } */
  (unsigned _Fract ()) ufp; /* { dg-error "cast specifies function type" } */
  (unsigned long _Fract ()) ulfp; /* { dg-error "cast specifies function type" } */
  (unsigned long long _Fract ()) ullfp; /* { dg-error "cast specifies function type" } */
  (_Sat short _Fract ()) Ssfp; /* { dg-error "cast specifies function type" } */
  (_Sat _Fract ()) Sfp; /* { dg-error "cast specifies function type" } */
  (_Sat long _Fract ()) Slfp; /* { dg-error "cast specifies function type" } */
  (_Sat long long _Fract ()) Sllfp; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned short _Fract ()) Susfp; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned _Fract ()) Sufp; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned long _Fract ()) Sulfp; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned long long _Fract ()) Sullfp; /* { dg-error "cast specifies function type" } */
  (short _Accum ()) sap; /* { dg-error "cast specifies function type" } */
  (_Accum ()) ap; /* { dg-error "cast specifies function type" } */
  (long _Accum ()) lap; /* { dg-error "cast specifies function type" } */
  (long long _Accum ()) llap; /* { dg-error "cast specifies function type" } */
  (unsigned short _Accum ()) usap; /* { dg-error "cast specifies function type" } */
  (unsigned _Accum ()) uap; /* { dg-error "cast specifies function type" } */
  (unsigned long _Accum ()) ulap; /* { dg-error "cast specifies function type" } */
  (unsigned long long _Accum ()) ullap; /* { dg-error "cast specifies function type" } */
  (_Sat short _Accum ()) Ssap; /* { dg-error "cast specifies function type" } */
  (_Sat _Accum ()) Sap; /* { dg-error "cast specifies function type" } */
  (_Sat long _Accum ()) Slap; /* { dg-error "cast specifies function type" } */
  (_Sat long long _Accum ()) Sllap; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned short _Accum ()) Susap; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned _Accum ()) Suap; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned long _Accum ()) Sulap; /* { dg-error "cast specifies function type" } */
  (_Sat unsigned long long _Accum ()) Sullap; /* { dg-error "cast specifies function type" } */

  (struct ssf) sf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sf) f; /* { dg-error "conversion to non-scalar type requested" } */
  (struct slf) lf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sllf) llf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct susf) usf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct suf) uf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sulf) ulf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sullf) ullf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSsf) Ssf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSf) Sf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSlf) Slf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSllf) Sllf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSusf) Susf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSuf) Suf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSulf) Sulf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSullf) Sullf; /* { dg-error "conversion to non-scalar type requested" } */
  (struct ssa) sa; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sa) a; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sla) la; /* { dg-error "conversion to non-scalar type requested" } */
  (struct slla) lla; /* { dg-error "conversion to non-scalar type requested" } */
  (struct susa) usa; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sua) ua; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sula) ula; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sulla) ulla; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSsa) Ssa; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSa) Sa; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSla) Sla; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSlla) Slla; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSusa) Susa; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSua) Sua; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSula) Sula; /* { dg-error "conversion to non-scalar type requested" } */
  (struct sSulla) Sulla; /* { dg-error "conversion to non-scalar type requested" } */

  (union usf) sf;
  (union uf) f;
  (union ulf) lf;
  (union ullf) llf;
  (union uusf) usf;
  (union uuf) uf;
  (union uulf) ulf;
  (union uullf) ullf;
  (union uSsf) Ssf;
  (union uSf) Sf;
  (union uSlf) Slf;
  (union uSllf) Sllf;
  (union uSusf) Susf;
  (union uSuf) Suf;
  (union uSulf) Sulf;
  (union uSullf) Sullf;
  (union usa) sa;
  (union ua) a;
  (union ula) la;
  (union ulla) lla;
  (union uusa) usa;
  (union uua) ua;
  (union uula) ula;
  (union uulla) ulla;
  (union uSsa) Ssa;
  (union uSa) Sa;
  (union uSla) Sla;
  (union uSlla) Slla;
  (union uSusa) Susa;
  (union uSua) Sua;
  (union uSula) Sula;
  (union uSulla) Sulla;

  (union usf) f; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) lf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) llf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) usf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) uf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) ulf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) ullf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Ssf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Slf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sllf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Susf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Suf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sulf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sullf; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) sa; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) a; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) la; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) lla; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) usa; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) ua; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) ula; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) ulla; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Ssa; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sa; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sla; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Slla; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Susa; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sua; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sula; /* { dg-error "cast to union type from type not present in union" } */
  (union usf) Sulla; /* { dg-error "cast to union type from type not present in union" } */

  (short _Fract) assf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Fract) asf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long _Fract) aslf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long long _Fract) asllf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned short _Fract) asusf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned _Fract) asuf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long _Fract) asulf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long long _Fract) asullf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat short _Fract) asSsf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat _Fract) asSf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long _Fract) asSlf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long long _Fract) asSllf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned short _Fract) asSusf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned _Fract) asSuf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long _Fract) asSulf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long long _Fract) asSullf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (short _Accum) assa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Accum) asa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long _Accum) asla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long long _Accum) aslla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned short _Accum) asusa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned _Accum) asua; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long _Accum) asula; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long long _Accum) asulla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat short _Accum) asSsa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat _Accum) asSa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long _Accum) asSla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long long _Accum) asSlla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned short _Accum) asSusa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned _Accum) asSua; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long _Accum) asSula; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long long _Accum) asSulla; /* { dg-error "aggregate value used where a fixed-point was expected" } */

  (short _Fract) ausf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Fract) auf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long _Fract) aulf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long long _Fract) aullf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned short _Fract) auusf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned _Fract) auuf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long _Fract) auulf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long long _Fract) auullf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat short _Fract) auSsf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat _Fract) auSf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long _Fract) auSlf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long long _Fract) auSllf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned short _Fract) auSusf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned _Fract) auSuf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long _Fract) auSulf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long long _Fract) auSullf; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (short _Accum) ausa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Accum) aua; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long _Accum) aula; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (long long _Accum) aulla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned short _Accum) auusa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned _Accum) auua; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long _Accum) auula; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (unsigned long long _Accum) auulla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat short _Accum) auSsa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat _Accum) auSa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long _Accum) auSla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat long long _Accum) auSlla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned short _Accum) auSusa; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned _Accum) auSua; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long _Accum) auSula; /* { dg-error "aggregate value used where a fixed-point was expected" } */
  (_Sat unsigned long long _Accum) auSulla; /* { dg-error "aggregate value used where a fixed-point was expected" } */
}
