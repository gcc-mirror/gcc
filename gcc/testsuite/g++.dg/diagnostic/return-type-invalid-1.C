struct S1
{
  int S1();  // { dg-error "3:return type" }
  int ~S1();  // { dg-error "3:return type" }
  int operator int();  // { dg-error "3:return type" }
};

struct S2
{
  const int S2();  // { dg-error "3:return type" }
  const int ~S2();  // { dg-error "3:return type" }
  const int operator int();  // { dg-error "3:return type" }
};

struct S3
{
  volatile int S3();  // { dg-error "3:return type" }
  volatile int ~S3();  // { dg-error "3:return type" }
  volatile int operator int(); // { dg-error "3:return type" } 
};

struct S4
{
  const volatile int S4();  // { dg-error "3:return type" }
  const volatile int ~S4();  // { dg-error "3:return type" }
  const volatile int operator int();  // { dg-error "3:return type" }
};
