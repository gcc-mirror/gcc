/* Test that Objective-C exceptions cause an error with -fobjc-exceptions.  */
/* { dg-do compile } */

@class Object;

int dummy (int number, Object *o)
{
  @throw o;           /* { dg-error ".-fobjc-exceptions. is required to enable Objective-C exception syntax" } */

  @try {              /* Nothing, error has already been produced.  */
    number++;
    @throw o;         /* Nothing, error has already been produced.  */
  }
  @catch (id object)
    {
      number++;
      @throw;        /* Nothing, error has already been produced.  */
    }
  @finally
    {
      number++;
    }
  
  @synchronized (o)   /* Nothing, error has already been produced.  */
    {
      number++;
    }
  
  return number;
}
