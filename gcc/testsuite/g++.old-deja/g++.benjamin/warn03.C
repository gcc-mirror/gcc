// 980420 bkoz 
// from g++/15307, tests for -Wredundant-decls for decls
// Build don't link: 
// Special g++ Options: -Wredundant-decls

//shouldn't crash
extern unsigned char *foo5[]; 
extern unsigned char *foo5[]; 


