/* Test optional argument for interrupt and use_shadow_register_set
   attributes.  */
/* { dg-do compile } */
/* { dg-options "isa_rev>=2" } */
/* { dg-final { scan-assembler "e0:.*ins\t\\\$27,\\\$26,10,6.*\.end\te0" } } */
/* { dg-final { scan-assembler-times "mfc0\t\\\$26,\\\$13" 3 } } */
/* { dg-final { scan-assembler-times "mfc0\t\\\$27,\\\$14" 11 } } */
/* { dg-final { scan-assembler "v0:.*ins\t\\\$27,\\\$0,8,1.*\.end\tv0" } } */
/* { dg-final { scan-assembler "v1:.*ins\t\\\$27,\\\$0,8,2.*\.end\tv1" } } */
/* { dg-final { scan-assembler "v2:.*ins\t\\\$27,\\\$0,8,3.*\.end\tv2" } } */
/* { dg-final { scan-assembler "v3:.*ins\t\\\$27,\\\$0,8,4.*\.end\tv3" } } */
/* { dg-final { scan-assembler "v4:.*ins\t\\\$27,\\\$0,8,5.*\.end\tv4" } } */
/* { dg-final { scan-assembler "v5:.*ins\t\\\$27,\\\$0,8,6.*\.end\tv5" } } */
/* { dg-final { scan-assembler "v6:.*ins\t\\\$27,\\\$0,8,7.*\.end\tv6" } } */
/* { dg-final { scan-assembler "v7:.*ins\t\\\$27,\\\$0,8,8.*\.end\tv7" } } */

/* { dg-final { scan-assembler-times "rdpgpr\t\\\$sp,\\\$sp" 1 } } */
/* { dg-final { scan-assembler-not "s1:.*rdpgpr\t\\\$sp,\\\$sp.*\.end\ts1" } } */

NOMIPS16 void __attribute__ ((interrupt("eic"))) e0 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=sw0"))) v0 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=sw1"))) v1 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=hw0"))) v2 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=hw1"))) v3 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=hw2"))) v4 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=hw3"))) v5 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=hw4"))) v6 () { }
NOMIPS16 void __attribute__ ((interrupt("vector=hw5"))) v7 () { }

NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set)) s0 () { }
NOMIPS16 void __attribute__ ((interrupt, use_shadow_register_set("intstack"))) s1 () { }
