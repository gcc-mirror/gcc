struct S
{
  typedef int i __attribute__((unused)) = 1;  // { dg-error "15:typedef .i. is initialized" }
};

typedef int i __attribute__((unused)) = 1;  // { dg-error "13:typedef .i. is initialized" }
