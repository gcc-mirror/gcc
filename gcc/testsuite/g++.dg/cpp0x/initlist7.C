// PR c++/37932
// { dg-options "-std=c++0x -pedantic-errors" }

typedef enum { AA=1, BB=2 } my_enum;

typedef struct { my_enum a:4 ; unsigned b:28; } stru;

void foo (char c, my_enum x, int i)
{
  char arr[2] = {c+'0', 0};	// { dg-error "narrowing" }
  stru s = {x,0};
}
