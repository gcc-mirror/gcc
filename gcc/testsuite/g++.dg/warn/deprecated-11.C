// PR c++/71274
// { dg-options -Wdeprecated-declarations }

struct foo
{
   __attribute__ ((deprecated)) static const int a;
};
