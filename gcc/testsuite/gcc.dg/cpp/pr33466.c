/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* Test various invalid constant float suffixes made up of letters of
   valid suffixes.  These are invalid regardless of whether the target
   compiler supports decimal float or fixed-point types.  */

long double rh = 0.5rh;		/* { dg-error "invalid suffix" } */
long double rl = 0.5rl;		/* { dg-error "invalid suffix" } */
long double rll = 0.5rll;	/* { dg-error "invalid suffix" } */
long double kh = 0.5kh;		/* { dg-error "invalid suffix" } */
long double kl = 0.5kl;		/* { dg-error "invalid suffix" } */
long double kll = 0.5kll;	/* { dg-error "invalid suffix" } */
long double ru = 0.5ru;		/* { dg-error "invalid suffix" } */
long double urh = 0.5urh;	/* { dg-error "invalid suffix" } */
long double hur = 0.5hur;	/* { dg-error "invalid suffix" } */
long double hru = 0.5hru;	/* { dg-error "invalid suffix" } */
long double ruh = 0.5ruh;	/* { dg-error "invalid suffix" } */
long double rhu = 0.5rhu;	/* { dg-error "invalid suffix" } */
long double url = 0.5url;	/* { dg-error "invalid suffix" } */
long double lur = 0.5lur;	/* { dg-error "invalid suffix" } */
long double lru = 0.5lru;	/* { dg-error "invalid suffix" } */
long double rul = 0.5rul;	/* { dg-error "invalid suffix" } */
long double rlu = 0.5rlu;	/* { dg-error "invalid suffix" } */
long double urll = 0.5urll;	/* { dg-error "invalid suffix" } */
long double llur = 0.5llur;	/* { dg-error "invalid suffix" } */
long double llru = 0.5llru;	/* { dg-error "invalid suffix" } */
long double rull = 0.5rull;	/* { dg-error "invalid suffix" } */
long double rllu = 0.5rllu;	/* { dg-error "invalid suffix" } */
long double ku = 0.5ku;		/* { dg-error "invalid suffix" } */
long double ukh = 0.5ukh;	/* { dg-error "invalid suffix" } */
long double huk = 0.5huk;	/* { dg-error "invalid suffix" } */
long double hku = 0.5hku;	/* { dg-error "invalid suffix" } */
long double kuh = 0.5kuh;	/* { dg-error "invalid suffix" } */
long double khu = 0.5khu;	/* { dg-error "invalid suffix" } */
long double ukl = 0.5ukl;	/* { dg-error "invalid suffix" } */
long double luk = 0.5luk;	/* { dg-error "invalid suffix" } */
long double lku = 0.5lku;	/* { dg-error "invalid suffix" } */
long double kul = 0.5kul;	/* { dg-error "invalid suffix" } */
long double klu = 0.5klu;	/* { dg-error "invalid suffix" } */
long double ukll = 0.5ukll;	/* { dg-error "invalid suffix" } */
long double lluk = 0.5lluk;	/* { dg-error "invalid suffix" } */
long double llku = 0.5llku;	/* { dg-error "invalid suffix" } */
long double kull = 0.5kull;	/* { dg-error "invalid suffix" } */
long double kllu = 0.5kllu;	/* { dg-error "invalid suffix" } */
long double ld = 0.5ld;		/* { dg-error "invalid suffix" } */
long double fd = 0.5fd;		/* { dg-error "invalid suffix" } */
long double dk = 0.5dk;		/* { dg-error "invalid suffix" } */
long double dr = 0.5dr;		/* { dg-error "invalid suffix" } */
long double ddw = 0.5ddw;	/* { dg-error "invalid suffix" } */
long double ddq = 0.5ddq;	/* { dg-error "invalid suffix" } */
long double ddl = 0.5ddl;	/* { dg-error "invalid suffix" } */
long double ddf = 0.5ddf;	/* { dg-error "invalid suffix" } */
long double ddd = 0.5ddd;	/* { dg-error "invalid suffix" } */
long double dw = 0.5dw;		/* { dg-error "invalid suffix" } */
long double dq = 0.5dq;		/* { dg-error "invalid suffix" } */
long double wd = 0.5wd;		/* { dg-error "invalid suffix" } */
long double qd = 0.5qd;		/* { dg-error "invalid suffix" } */
long double wdd = 0.5wdd;	/* { dg-error "invalid suffix" } */
long double qdd = 0.5qdd;	/* { dg-error "invalid suffix" } */
long double ldd = 0.5ldd;	/* { dg-error "invalid suffix" } */
long double fdd = 0.5fdd;	/* { dg-error "invalid suffix" } */
long double ddi = 0.5ddi;	/* { dg-error "invalid suffix" } */
long double idd = 0.5idd;	/* { dg-error "invalid suffix" } */
