/* Test that 'in', 'out', 'inout', 'bycopy', 'byref', 'oneway'
   are not keywords outside of a "protocol qualifier" context.
*/
/* { dg-do compile } */

typedef int in;

in out (in inout)
{
  int byref = inout * 2;
  
  return byref + inout;
}

@class byref;

@interface inout
@end

@protocol oneway;

int main (void)
{
  in bycopy = (in)(out (0));

  return (in)bycopy;
}
