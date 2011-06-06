/*
    load-3.m

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: June  3, 2001

    Test if the +load methods are invoked, and are invoked in the
    proper order.
 */

#include <stdlib.h>
#include "../../objc-obj-c++-shared/TestsuiteObject.m"
#include <objc/objc.h>

@interface A : TestsuiteObject
@end

@interface B : A
@end

static a_load = 0;
static b_load = 0;
static a_category_load = 0;
static b_category_load = 0;

@implementation A (Category)
+ (void)load
{
  a_category_load = 1;
  printf("+[A(Category) load]\n");

  if (a_load != 1)
    {
      printf("+load for A(Category) invoked before A's!\n");
      abort();
    }
}
@end

@implementation B(Category)
+ (void)load
{
  b_category_load = 1;
  printf("+[B(Category) load]\n");

  if (b_load != 1)
    {
      printf ("+load for B(Category) invoked before B!\n");
      abort();
    }
}
@end

@implementation B
+ (void)load
{
  b_load = 1;
  printf("+[B load]\n");

  if (a_load != 1)
    {
      printf("+load for B invoked before A's!\n");
      abort();
    }

  if (b_category_load != 0)
    {
      printf("+load for B invoked after B(Category)!\n");
      abort();
    }
}
@end

@implementation A
+ (void)load
{
  a_load = 1;
  printf("+[A load]\n");

  if (a_category_load != 0)
    {
      printf("+load for A(Category) invoked before A!\n");
      abort();
    }

  if (b_load != 0)
    {
      printf("+load for A invoked after B!\n");
      abort();
    }

  if (b_category_load != 0)
    {
      printf("+load for B(Category) invoked before A and B!\n");
      abort();
    }
}
@end

int main (void)
{
  if (a_load + b_load + a_category_load + b_category_load != 4)
    {
      printf("Not all +load methods invoked!\n");
      abort();
    }

  return 0;
}
