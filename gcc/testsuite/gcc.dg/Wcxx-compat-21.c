/* PR c/44772 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

typedef enum { E1, E2 } E;

typedef struct
{
  E e;
  union
  {
    int i;
    char *c;
  };			/* { dg-bogus "as both field and typedef name" } */
} S;

S s;

typedef int T;

struct U
{
  T t;
  union { int i; };	/* { dg-bogus "as both field and typedef name" } */
};
