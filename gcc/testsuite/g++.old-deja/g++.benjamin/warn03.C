// { dg-do assemble  }
// { dg-options "-Wredundant-decls" }
// 980420 bkoz 
// from g++/15307, tests for -Wredundant-decls for decls

//shouldn't crash
extern unsigned char *foo5[]; 
extern unsigned char *foo5[]; 


