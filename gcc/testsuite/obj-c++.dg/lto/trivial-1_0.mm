/* { dg-lto-do run } */
/* { dg-skip-if "Needs OBJC2 ABI" { "*-*-darwin*" && lp64 } { "*" } { "" } } */
extern "C" {
extern int printf (char *,...) ;
extern void abort (void) ;
} 

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
  Class cl = [myRootObject class];
  if (cl != (Class)0) {
    printf((char *)"trivial OK\n");
    return 0;
  }
  abort () ;
}
