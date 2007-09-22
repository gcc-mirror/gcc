/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

typedef _Fract qq  __attribute__ ((mode (QQ)));
typedef _Fract hq  __attribute__ ((mode (HQ)));
typedef _Fract sq  __attribute__ ((mode (SQ)));
typedef _Fract dq  __attribute__ ((mode (DQ)));
typedef unsigned _Fract uqq  __attribute__ ((mode (UQQ)));
typedef unsigned _Fract uhq  __attribute__ ((mode (UHQ)));
typedef unsigned _Fract usq  __attribute__ ((mode (USQ)));
typedef unsigned _Fract udq  __attribute__ ((mode (UDQ)));
typedef _Sat _Fract Sqq  __attribute__ ((mode (QQ)));
typedef _Sat _Fract Shq  __attribute__ ((mode (HQ)));
typedef _Sat _Fract Ssq  __attribute__ ((mode (SQ)));
typedef _Sat _Fract Sdq  __attribute__ ((mode (DQ)));
typedef _Sat unsigned _Fract Suqq  __attribute__ ((mode (UQQ)));
typedef _Sat unsigned _Fract Suhq  __attribute__ ((mode (UHQ)));
typedef _Sat unsigned _Fract Susq  __attribute__ ((mode (USQ)));
typedef _Sat unsigned _Fract Sudq  __attribute__ ((mode (UDQ)));
typedef _Accum ha  __attribute__ ((mode (HA)));
typedef _Accum sa  __attribute__ ((mode (SA)));
typedef _Accum da  __attribute__ ((mode (DA)));
typedef unsigned _Accum uha  __attribute__ ((mode (UHA)));
typedef unsigned _Accum usa  __attribute__ ((mode (USA)));
typedef unsigned _Accum uda  __attribute__ ((mode (UDA)));
typedef _Sat _Accum Sha  __attribute__ ((mode (HA)));
typedef _Sat _Accum Ssa  __attribute__ ((mode (SA)));
typedef _Sat _Accum Sda  __attribute__ ((mode (DA)));
typedef _Sat unsigned _Accum Suha  __attribute__ ((mode (UHA)));
typedef _Sat unsigned _Accum Susa  __attribute__ ((mode (USA)));
typedef _Sat unsigned _Accum Suda  __attribute__ ((mode (UDA)));

/* Not all platforms support TQ, UTQ, TA, UTA modes.  */
#if defined(__LP64__) && !defined(__hppa__)
typedef _Fract tq  __attribute__ ((mode (TQ)));
typedef unsigned _Fract utq  __attribute__ ((mode (UTQ)));
typedef _Sat _Fract Stq  __attribute__ ((mode (TQ)));
typedef _Sat unsigned _Fract Sutq  __attribute__ ((mode (UTQ)));
typedef _Accum ta  __attribute__ ((mode (TA)));
typedef unsigned _Accum uta  __attribute__ ((mode (UTA)));
typedef _Sat _Accum Sta  __attribute__ ((mode (TA)));
typedef _Sat unsigned _Accum Suta  __attribute__ ((mode (UTA)));

int tqsize[sizeof (tq) == 16 ? 1 : -1];
int utqsize[sizeof (utq) == 16 ? 1 : -1];
int Stqsize[sizeof (Stq) == 16 ? 1 : -1];
int Sutqsize[sizeof (Sutq) == 16 ? 1 : -1];
int tasize[sizeof (ta) == 16 ? 1 : -1];
int utasize[sizeof (uta) == 16 ? 1 : -1];
int Stasize[sizeof (Sta) == 16 ? 1 : -1];
int Sutasize[sizeof (Suta) == 16 ? 1 : -1];

int tqalign = __alignof (tq);
int utqalign = __alignof (utq);
int Stqalign = __alignof (Stq);
int Sutqalign = __alignof (Sutq);
int taalign = __alignof (ta);
int utaalign = __alignof (uta);
int Staalign = __alignof (Sta);
int Sutaalign = __alignof (Suta);
#endif

int qqsize[sizeof (qq) == 1 ? 1 : -1];
int hqsize[sizeof (hq) == 2 ? 1 : -1];
int sqsize[sizeof (sq) == 4 ? 1 : -1];
int dqsize[sizeof (dq) == 8 ? 1 : -1];
int uqqsize[sizeof (qq) == 1 ? 1 : -1];
int uhqsize[sizeof (hq) == 2 ? 1 : -1];
int usqsize[sizeof (sq) == 4 ? 1 : -1];
int udqsize[sizeof (dq) == 8 ? 1 : -1];
int Sqqsize[sizeof (Sqq) == 1 ? 1 : -1];
int Shqsize[sizeof (Shq) == 2 ? 1 : -1];
int Ssqsize[sizeof (Ssq) == 4 ? 1 : -1];
int Sdqsize[sizeof (Sdq) == 8 ? 1 : -1];
int Suqqsize[sizeof (Sqq) == 1 ? 1 : -1];
int Suhqsize[sizeof (Shq) == 2 ? 1 : -1];
int Susqsize[sizeof (Ssq) == 4 ? 1 : -1];
int Sudqsize[sizeof (Sdq) == 8 ? 1 : -1];
int hasize[sizeof (ha) == 2 ? 1 : -1];
int sasize[sizeof (sa) == 4 ? 1 : -1];
int dasize[sizeof (da) == 8 ? 1 : -1];
int uhasize[sizeof (uha) == 2 ? 1 : -1];
int usasize[sizeof (usa) == 4 ? 1 : -1];
int udasize[sizeof (uda) == 8 ? 1 : -1];
int Shasize[sizeof (Sha) == 2 ? 1 : -1];
int Ssasize[sizeof (Ssa) == 4 ? 1 : -1];
int Sdasize[sizeof (Sda) == 8 ? 1 : -1];
int Suhasize[sizeof (Suha) == 2 ? 1 : -1];
int Susasize[sizeof (Susa) == 4 ? 1 : -1];
int Sudasize[sizeof (Suda) == 8 ? 1 : -1];

int qqalign = __alignof (qq);
int hqalign = __alignof (hq);
int sqalign = __alignof (sq);
int dqalign = __alignof (dq);
int uqqalign = __alignof (uqq);
int uhqalign = __alignof (uhq);
int usqalign = __alignof (usq);
int udqalign = __alignof (udq);
int Sqqalign = __alignof (Sqq);
int Shqalign = __alignof (Shq);
int Ssqalign = __alignof (Ssq);
int Sdqalign = __alignof (Sdq);
int Suqqalign = __alignof (Suqq);
int Suhqalign = __alignof (Suhq);
int Susqalign = __alignof (Susq);
int Sudqalign = __alignof (Sudq);
int haalign = __alignof (ha);
int saalign = __alignof (sa);
int daalign = __alignof (da);
int uhaalign = __alignof (uha);
int usaalign = __alignof (usa);
int udaalign = __alignof (uda);
int Shaalign = __alignof (Sha);
int Ssaalign = __alignof (Ssa);
int Sdaalign = __alignof (Sda);
int Suhaalign = __alignof (Suha);
int Susaalign = __alignof (Susa);
int Sudaalign = __alignof (Suda);
