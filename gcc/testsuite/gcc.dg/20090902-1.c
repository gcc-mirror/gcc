/* { dg-do compile } */
#define STRING(x) #x
char buf[] = STRING(L'\x123');
