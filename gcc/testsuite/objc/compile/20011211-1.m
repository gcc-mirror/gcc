typedef struct objc_class *Class;

typedef struct objc_object {
  Class isa;
} *id;

@interface nsset
+ (id)set;
@end

@interface baz
- (void)set;
@end

nsset *fn ()
{
  nsset *bar;
  bar = [nsset set];
}
