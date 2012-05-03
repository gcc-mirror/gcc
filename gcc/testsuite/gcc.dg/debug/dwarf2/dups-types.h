struct A
{
  int i;
  int j;
};

typedef struct A A2;
extern A2 a;

A2 f(A2);
