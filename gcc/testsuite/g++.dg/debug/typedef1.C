// PR debug/6436
// { dg-do compile }

typedef struct 
{
  unsigned int a0, a1;
} A __attribute__ ((aligned(8)));

typedef struct
{
  A a;
} B;

struct C
{
  B *bp;
};
