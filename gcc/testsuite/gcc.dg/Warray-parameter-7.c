/* PR c/97206 - ICE in composite_type on declarations of a similar array types
   { dg-do compile }
   { dg-options "-Wall" } */

__attribute__((__access__(__write_only__, 1))) void
f1 (char* restrict);

void f1 (char*);

char a1[];
char a1[] = { };


void f2 (char[restrict]);
void f2 (char*);

char a2[];
char a2[] = { };


void f3 (char*);
void f3 (char[const]);

extern const char a3[];
extern const char a3[1];
