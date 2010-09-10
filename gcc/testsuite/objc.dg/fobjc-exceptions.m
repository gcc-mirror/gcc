/* Test that Objective-C exceptions cause an error with -fobjc-exceptions.  */
/* { dg-do compile } */

@class Object;

int dummy (int number, Object *o)
{
  @try {            /* { dg-error "fobjc-exceptions" "is required to enable Objective-C exception syntax" } */
    number++;
    @throw o;     /* { dg-error "fobjc-exceptions" "is required to enable Objective-C exception syntax" } */
  }
  @catch (id object)
    {
      number++;
      @throw;       /* { dg-error "fobjc-exceptions" "is required to enable Objective-C exception syntax" } */
    }
  @finally
    {
      number++;
    }
  
  @synchronized (o) /* { dg-error "fobjc-exceptions" "is required to enable Objective-C exception syntax" } */
    {
      number++;
    }
  
  return number;
}

