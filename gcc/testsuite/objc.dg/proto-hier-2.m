/* Test protocol warning.  */
/* Contributed by Devang Patel <dpatel@apple.com>.  */
/* { dg-do compile } */

typedef struct objc_object { struct objc_class *class_pointer; } *id;

@protocol Bar
@end

id <Bar> Foo_Bar () { }

typedef struct
{
        int i;
} MyStruct;

@interface Foo
{
  id _mainData;
  MyStruct *_anotherData;
}

-(id) mainDataSource;
-(id) anotherDataSource;
-(id) my_method: (int) i;
@end

@implementation Foo
-(id) anotherDataSource
{
        return (id)_anotherData;
}

-(id) mainDataSource
{
        return _mainData;
}

-(id) my_method: (int) i
{
  id one = [self anotherDataSource];

  i = i - 1;
  // Do not issue warning about my_method not found in protocol
  return [(one ? [self mainDataSource] : one) my_method:i];
}

@end

