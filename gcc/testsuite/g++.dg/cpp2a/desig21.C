// PR c++/55227 
// Test designated initializer for char array by string constant

// { dg-options "" }

struct C {char a[2];};

/* Case a, designated, unbraced, string-literal of the exact same size
   as the initialized char array; valid and accepted before and after.  */
C a = {.a="a"};

/* Cases b,c,d, designated, braced or mimatched-size, string literal,
   previously rejected; "C99 designator 'a' outside aggregate initializer".  */
C b = {.a=""};
C c = {.a={""}};
C d = {.a={"a"}};

/* Case e, designated char array field and braced, designated array element(s)
   (with GNU [N]= extension) valid and accepted before and after.  */
C e = {.a={[0]='a'}};

/* Cases f,g,h, braced string literal, 'designated' within inner braces;
   invalid, previously accepted as positional with 'designator' ignored.  */
C f = {{[0]="a"}}; // { dg-error "C99 designator .0. outside aggregate initializer" }
C g = {{.a="a"}}; // { dg-error "C99 designator .a. outside aggregate initializer" }
C h = {{.b="a"}}; // { dg-error "C99 designator .b. outside aggregate initializer" }

char a2[][10] = { [0] = { "aaa" } };

struct D { C c; int a[8]; };

D x = { .c {.a={"a"}}, .a={1,2,3,4,5,6,7,8} };

struct A { union { int a; char c[4]; }; };

A non = { .c = "c++" };

template <class T>
void t()
{
  C ca[] = { {.a=""}, {.a={""}}, };

}

void u()
{
  return t<void>();
}
