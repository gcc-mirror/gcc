/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-options "-Wall" } */
/* { dg-do compile } */

/* Test that fast enumeration loops where the iterating variable is declared
   but not used do not generate warnings.  */

/*
struct __objcFastEnumerationState
{
  unsigned long state;
  id            *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
};
*/
@interface Object
{
  Class isa;
}
- (unsigned long)countByEnumeratingWithState: (struct __objcFastEnumerationState *)state
                                     objects:(id *)stackbuf 
                                       count:(unsigned int)len;
- (id) enumerator;
- (Class) classEnumerator;
@end

unsigned int count_objects_in_collection (id collection)
{
  unsigned int count = 0;

  /* The following line should generate no warnings even with
     -Wall.  */
  for (id object in collection)
    count++;

  return count;
}

unsigned int count_objects_in_collection_2 (id collection)
{
  unsigned int count = 0;
  id object;

  /* The following line should generate no warnings even with
     -Wall.  */
  for (object in collection)
    count++;

  return count;
}
