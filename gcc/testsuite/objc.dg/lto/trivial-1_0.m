/* { dg-lto-do run } */
/* { dg-skip-if "" { "*-*-darwin*" && lp64 } { "*" } { "" } } */
extern int printf (char *,...) ;

typedef struct objc_class *Class;

struct objc_class {
    Class isa;
    /* other stuff... */
}  ;

@interface myRootObject {
@public
     Class isa;
}
+initialize;
+(Class)class;

@end

@implementation myRootObject
+initialize {
     return self;
}

+(Class)class {
     return (Class)self;
}

@end

int main(void)
{
  [myRootObject class];
  printf("trivial OK\n");
  return 0;
}
