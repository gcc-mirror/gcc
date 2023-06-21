/* { dg-options "(-mips16) isa_rev<=5" } */

 __attribute__((code_readable ("magic"))) int foo () {} /* { dg-warning "argument to 'code_readable' attribute is neither no, pcrel nor yes" } */

 __attribute__((code_readable (1))) int * bar () {} /* { dg-warning "'code_readable' attribute requires a string argument" } */
