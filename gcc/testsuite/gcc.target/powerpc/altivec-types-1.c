/* { dg-do compile { target powerpc*-*-linux* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Valid AltiVec vector types should be accepted with no warnings.  */

__vector char vc;
__vector unsigned char vuc;
__vector signed char vsc;
__vector __bool char vbc;
__vector short vh;
__vector signed short vsh;
__vector unsigned short vuh;
__vector short int vhi;
__vector signed short int vshi;
__vector unsigned short int vuhi;
__vector __bool short vbh;
__vector __bool short int vbhi;
__vector int vi;
__vector unsigned int vui;
__vector signed int vsi;
__vector __bool int vbi;
__vector unsigned vuj;
__vector signed vsj;
__vector __bool vbj;
__vector float vf;

/* These should be rejected as invalid AltiVec types.  */

__vector long long vll;			/* { dg-error "AltiVec types" "" } */
__vector unsigned long long vull;	/* { dg-error "AltiVec types" "" } */
__vector signed long long vsll;		/* { dg-error "AltiVec types" "" } */
__vector __bool long long vbll;		/* { dg-error "AltiVec types" "" } */
__vector long long int vlli;		/* { dg-error "AltiVec types" "" } */
__vector unsigned long long int vulli;	/* { dg-error "AltiVec types" "" } */
__vector signed long long int vslli;	/* { dg-error "AltiVec types" "" } */
__vector __bool long long int vblli;	/* { dg-error "AltiVec types" "" } */
__vector double vd1;			/* { dg-error "AltiVec types" "" } */
__vector long double vld;		/* { dg-error "AltiVec types" "" } */
__vector _Bool vb;			/* { dg-error "AltiVec types" "" } */
__vector _Complex float vcf;		/* { dg-error "AltiVec types" "" } */
__vector _Complex double vcd;		/* { dg-error "AltiVec types" "" } */
__vector _Complex long double vcld;	/* { dg-error "AltiVec types" "" } */
__vector _Complex signed char vcsc;	/* { dg-error "AltiVec types" "" } */
__vector _Complex unsigned char vcuc;	/* { dg-error "AltiVec types" "" } */
__vector _Complex short vcss;		/* { dg-error "AltiVec types" "" } */
__vector _Complex unsigned short vcus;	/* { dg-error "AltiVec types" "" } */
__vector _Complex int vcsi;		/* { dg-error "AltiVec types" "" } */
__vector _Complex unsigned int vcui;	/* { dg-error "AltiVec types" "" } */
__vector _Complex long vcsl;		/* { dg-error "AltiVec types" "" } */
__vector _Complex unsigned long vcul;	/* { dg-error "AltiVec types" "" } */
__vector _Complex long long vcsll;	/* { dg-error "AltiVec types" "" } */
__vector _Complex unsigned long long vcull; /* { dg-error "AltiVec types" "" } */
__vector __complex float v_cf;		/* { dg-error "AltiVec types" "" } */
__vector __complex double v_cd;		/* { dg-error "AltiVec types" "" } */
__vector __complex long double v_cld;	/* { dg-error "AltiVec types" "" } */
__vector __complex signed char v_csc;	/* { dg-error "AltiVec types" "" } */
__vector __complex unsigned char v_cuc;	/* { dg-error "AltiVec types" "" } */
__vector __complex short v_css;		/* { dg-error "AltiVec types" "" } */
__vector __complex unsigned short v_cus; /* { dg-error "AltiVec types" "" } */
__vector __complex int v_csi;		/* { dg-error "AltiVec types" "" } */
__vector __complex unsigned int v_cui;	/* { dg-error "AltiVec types" "" } */
__vector __complex long v_csl;		/* { dg-error "AltiVec types" "" } */
__vector __complex unsigned long v_cul;	/* { dg-error "AltiVec types" "" } */
__vector __complex long long v_csll;	/* { dg-error "AltiVec types" "" } */
__vector __complex unsigned long long v_cull; /* { dg-error "AltiVec types" "" } */

/* These should be rejected because the component types are invalid.  We
   don't care about the actual error messages here.  */

__vector __bool unsigned char vbuc;	/* { dg-error "" "" } */
__vector __bool signed char vbsc;	/* { dg-error "" "" } */
__vector __bool unsigned short vbuh;	/* { dg-error "" "" } */
__vector __bool signed short vbsh;	/* { dg-error "" "" } */
__vector __bool unsigned int vbui;	/* { dg-error "" "" } */
__vector __bool signed int vbsi;	/* { dg-error "" "" } */
__vector __bool unsigned vbuj;		/* { dg-error "" "" } */
__vector __bool signed vbsj;		/* { dg-error "" "" } */
__vector signed float vsf;		/* { dg-error "" "" } */
__vector unsigned float vuf;		/* { dg-error "" "" } */
__vector short float vsf;		/* { dg-error "" "" } */
__vector signed double vsd;		/* { dg-error "" "" } */
__vector unsigned double vud;		/* { dg-error "" "" } */
__vector short double vsd;		/* { dg-error "" "" } */
__vector __bool float vbf;		/* { dg-error "" "" } */
__vector __bool double vbd;		/* { dg-error "" "" } */
__vector __bool short float blf;	/* { dg-error "" "" } */
__vector __bool short double vlbd;	/* { dg-error "" "" } */
