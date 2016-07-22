// PR c++/66644
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-pedantic" }

struct test1  
{
  union
  {
    struct { char a=0, b=0; };
    char buffer[16];
  };
};

struct test2 
{
  union  
  {
    struct { char a=0, b; };
    char buffer[16];
  };
};

struct test3
{
  union
  {
    struct { char a, b; } test2{0,0};
    char buffer[16];
  };
};

struct test4
{
  union  
  {   // { dg-error "multiple fields" }
    struct { char a=0, b=0; };
    struct { char c=0, d; };
  };
};

struct test5
{
  union
  {
    union { char a=0, b=0; };  // { dg-error "multiple fields" }
    char buffer[16];
  };
};
