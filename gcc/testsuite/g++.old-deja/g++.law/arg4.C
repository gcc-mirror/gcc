// { dg-do assemble  }
// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <14t4tyk@rpi.edu>
// From: jorgej@colossus.cs.rpi.edu (Joaquim Jorge)
// Subject: g++ 2.3.3 Doesn't check function types in initializer lists ?
// Date: Tue, 9 Mar 1993 21:39:08 GMT

typedef void (*FuncPtr)(int a, float b);
class Amazing { int a; int b; int c; };

extern void *Wrong1(char *a, int *b);
extern void *Wrong2(Amazing a, int *b);
extern void *Wrong3(char *a, Amazing *b);
extern void Wrong4(char *a, int *b);
extern Amazing Wrong5(char *a, int *b);

FuncPtr p = &Wrong5;// { dg-error "" } .*
FuncPtr func_ptr_array[] = { &Wrong1, &Wrong2, &Wrong3, &Wrong4, &Wrong5, 0 };// { dg-error "" } .*

