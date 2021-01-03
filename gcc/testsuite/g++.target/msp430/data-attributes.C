/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } } */
/* { dg-options "-mlarge" } */

/* The msp430-specific variable attributes "upper", either", "noinit"
   and "persistent", all conflict with one another.
   "lower" can be used to indicate that a variable with a section set by the
   "section", "noinit", or "persistent" attributes is in lower memory, so it
   does not conflict with these.
   These attributes also conflict with the "section" attribute, since they
   specify sections to put the variables into.  */
int __attribute__((persistent)) p = 10;
int __attribute__((persistent,lower)) pl = 20;
int __attribute__((persistent,upper)) pu = 20; /* { dg-warning "ignoring attribute 'upper' because it conflicts with attribute 'persistent'" } */
int __attribute__((persistent,either)) pe = 20; /* { dg-warning "ignoring attribute 'either' because it conflicts with attribute 'persistent'" } */
/* This one results in an error because the handler for persistent sets the
   section to .persistent there and then.  */
int __attribute__((persistent,section(".data.foo"))) ps = 20; /* { dg-warning "ignoring attribute 'section' because it conflicts with attribute 'persistent'" } */
int __attribute__((persistent,noinit)) pn = 2; /* { dg-warning "ignoring attribute 'noinit' because it conflicts with attribute 'persistent'" } */
int __attribute__((persistent)) zz; /* { dg-warning "ignoring 'persistent' attribute set on uninitialized variable" } */

int __attribute__((noinit)) n;
int __attribute__((noinit,lower)) nl;
int __attribute__((noinit,upper)) nu; /* { dg-warning "ignoring attribute 'upper' because it conflicts with attribute 'noinit'" } */
int __attribute__((noinit,either)) ne; /* { dg-warning "ignoring attribute 'either' because it conflicts with attribute 'noinit'" } */
int __attribute__((noinit,persistent)) np; /* { dg-warning "ignoring attribute 'persistent' because it conflicts with attribute 'noinit'" } */
int __attribute__((noinit,section(".data.foo"))) ns; /* { dg-warning "ignoring attribute 'section' because it conflicts with attribute 'noinit'" } */

int __attribute__((lower)) l = 20;
int __attribute__((lower,upper)) lu = 20; /* { dg-warning "ignoring attribute 'upper' because it conflicts with attribute 'lower'" } */
int __attribute__((lower,either)) le = 20; /* { dg-warning "ignoring attribute 'either' because it conflicts with attribute 'lower'" } */
int __attribute__((lower,persistent)) lp = 20;
int __attribute__((lower,noinit)) ln;
int __attribute__((lower,section(".data.foo"))) ls = 30;

int __attribute__((upper)) u = 20;
int __attribute__((upper,lower)) ul = 20; /* { dg-warning "ignoring attribute 'lower' because it conflicts with attribute 'upper'" } */
int __attribute__((upper,either)) ue = 20; /* { dg-warning "ignoring attribute 'either' because it conflicts with attribute 'upper'" } */
int __attribute__((upper,persistent)) up = 20; /* { dg-warning "ignoring attribute 'persistent' because it conflicts with attribute 'upper'" } */
int __attribute__((upper,noinit)) un; /* { dg-warning "ignoring attribute 'noinit' because it conflicts with attribute 'upper'" } */
int __attribute__((upper,section(".data.foo"))) us = 30; /* { dg-warning "ignoring attribute 'section' because it conflicts with attribute 'upper'" } */

int __attribute__((either)) e = 20;
int __attribute__((either,lower)) el = 20; /* { dg-warning "ignoring attribute 'lower' because it conflicts with attribute 'either'" } */
int __attribute__((either,upper)) ee = 20; /* { dg-warning "ignoring attribute 'upper' because it conflicts with attribute 'either'" } */
int __attribute__((either,persistent)) ep = 20; /* { dg-warning "ignoring attribute 'persistent' because it conflicts with attribute 'either'" } */
int __attribute__((either,noinit)) en; /* { dg-warning "ignoring attribute 'noinit' because it conflicts with attribute 'either'" } */
int __attribute__((either,section(".data.foo"))) es = 30; /* { dg-warning "ignoring attribute 'section' because it conflicts with attribute 'either'" } */

int __attribute__((section(".data.foo"))) s = 20;
int __attribute__((section(".data.foo"),noinit)) sn; /* { dg-warning "ignoring attribute 'noinit' because it conflicts with attribute 'section'" } */
int __attribute__((section(".data.foo"),persistent)) sp = 20; /* { dg-warning "ignoring attribute 'persistent' because it conflicts with attribute 'section'" } */
int __attribute__((section(".data.foo"),lower)) sl = 2;
int __attribute__((section(".data.foo"),upper)) su = 20; /* { dg-warning "ignoring attribute 'upper' because it conflicts with attribute 'section'" } */
int __attribute__((section(".data.foo"),either)) se = 2; /* { dg-warning "ignoring attribute 'either' because it conflicts with attribute 'section'" } */
