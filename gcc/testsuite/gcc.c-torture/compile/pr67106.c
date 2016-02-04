/* { dg-options "-g -fpack-struct" } */
typedef struct S S;

struct S
{
  struct
  {
    S *s;
  };
  int a;
};

