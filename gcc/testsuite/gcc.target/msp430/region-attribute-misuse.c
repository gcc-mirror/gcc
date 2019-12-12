/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" "-mlarge" "-mcode-region=*" "-mdata-region=*" } { "" } } */
/* { dg-final { scan-assembler-not ".section.*bss" } } */
/* { dg-final { scan-assembler ".section.*upper.data" } } */
/* { dg-final { scan-assembler ".section.*lower.data" } } */
/* { dg-final { scan-assembler ".section.*either.data" } } */

int __attribute__((upper)) upper_bss; /* { dg-warning "'upper' attribute ignored.  Large memory model .'-mlarge'. is required" } */
int __attribute__((lower)) lower_bss; /* { dg-warning "'lower' attribute ignored.  Large memory model .'-mlarge'. is required" } */
int __attribute__((either)) either_bss; /* { dg-warning "'either' attribute ignored.  Large memory model .'-mlarge'. is required" } */

/* Verify that even without -mlarge, objects can still be placed in
   upper/lower/either regions manually.  */
int __attribute__((section(".upper.data"))) upper_data = 1;
int __attribute__((section(".lower.data"))) lower_data = 2;
int __attribute__((section(".either.data"))) either_data = 3;
