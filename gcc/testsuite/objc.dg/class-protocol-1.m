/* Check Class <protocol> types */
/* Author: David Ayers <d.ayers@inode.at> */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>
#include "../objc-obj-c++-shared/runtime.h"

@protocol MyProto1
+(void)doItClass1;
-(void)doItInstance1;
@end

@protocol MyProto2
+(void)doItClass2;
-(void)doItInstance2;
@end

@interface MyClass1 <MyProto1>
{
  Class isa;
}
@end
@implementation MyClass1
+(void)doItClass1{}
-(void)doItInstance1{}
@end

@interface MyClass2 : MyClass1 <MyProto2>
@end
@implementation MyClass2
+(void)doItClass2{}
-(void)doItInstance2{}
@end

@interface MyClass3
{
  Class isa;
}
@end
@interface MyClass4 : MyClass3 <MyProto1>
@end

/*----------------------------------------*/

Class cls = 0;
Class <MyProto1> clsP1 = 0;
Class <MyProto2> clsP2 = 0;

void
testSimple(void)
{
  [cls doItClass1];
  [cls doItInstance1];
  [cls doItClass2];
  [cls doItInstance2];

  [clsP1 doItClass1];
  [clsP1 doItInstance1]; /* { dg-warning "instead of" }  */
  [clsP1 doItClass2];    /* { dg-warning "not found in protocol" } */
  [clsP1 doItInstance2]; /* { dg-warning "not found in protocol" } */

  [clsP2 doItClass1];    /* { dg-warning "not found in protocol" } */
  [clsP2 doItInstance1]; /* { dg-warning "not found in protocol" } */
  [clsP2 doItClass2];
  [clsP2 doItInstance2]; /* { dg-warning "instead of" }  */

  [MyClass1 doItClass1];
  [MyClass1 doItInstance1];
  [MyClass1 doItClass2];    /* { dg-warning "may not respond to" } */
  [MyClass1 doItInstance2]; /* { dg-warning "may not respond to" } */

  [MyClass2 doItClass1];
  [MyClass2 doItInstance1];
  [MyClass2 doItClass2];
  [MyClass2 doItInstance2]; /* { dg-warning "may not respond to" } */

  [MyClass3 doItClass1];    /* { dg-warning "may not respond to" } */
  [MyClass3 doItInstance1]; /* { dg-warning "may not respond to" } */

  [MyClass4 doItClass1];
  [MyClass4 doItInstance1]; /* { dg-warning "may not respond to" } */
}

/*----------------------------------------*/
/* Protocols declared by categories */

@protocol MyProto3
+(void)doItClass3;
-(void)doItInstance3;
@end
@protocol MyProto4
+(void)doItClass4;
-(void)doItInstance4;
@end

@interface MyClass1 (Category1) <MyProto3>
@end
@interface MyClass2 (Category2) <MyProto4>
@end

void
testCategory(void)
{
  [cls doItClass3];
  [cls doItInstance3];
  [cls doItClass4];
  [cls doItInstance4];

  [MyClass1 doItClass3];
  [MyClass1 doItInstance3];
  [MyClass1 doItClass4];    /* { dg-warning "may not respond" } */
  [MyClass1 doItInstance4]; /* { dg-warning "may not respond" } */

  [MyClass2 doItClass3];
  [MyClass2 doItInstance3];
  [MyClass2 doItClass4];
  [MyClass2 doItInstance4]; /* { dg-warning "may not respond" } */

}

/*----------------------------------------*/
/* Inherited protocols declared by categories */

@protocol MyProto5 <MyProto1>
+(void)doItClass5;
-(void)doItInstance5;
@end

@protocol MyProto6 <MyProto2>
+(void)doItClass6;
-(void)doItInstance6;
@end

@interface MyClass1 (Category3) <MyProto5>
@end
@interface MyClass2 (Category4) <MyProto6>
@end

Class <MyProto5> clsP5 = 0;
Class <MyProto6> clsP6 = 0;

void
testCategoryInherited(void)
{
  [cls doItClass5];
  [cls doItInstance5];
  [cls doItClass6];
  [cls doItInstance6];

  [clsP5 doItClass1];
  [clsP5 doItInstance1]; /* { dg-warning "instead of" }  */
  [clsP5 doItClass2];    /* { dg-warning "not found in protocol" } */
  [clsP5 doItInstance2]; /* { dg-warning "not found in protocol" } */

  [clsP6 doItClass1];    /* { dg-warning "not found in protocol" } */
  [clsP6 doItInstance1]; /* { dg-warning "not found in protocol" } */
  [clsP6 doItClass2];
  [clsP6 doItInstance2]; /* { dg-warning "instead of" }  */


  [MyClass1 doItClass5];
  [MyClass1 doItInstance5];
  [MyClass1 doItClass6];    /* { dg-warning "may not respond" } */
  [MyClass1 doItInstance6]; /* { dg-warning "may not respond" } */

  [MyClass2 doItClass5];
  [MyClass2 doItInstance5];
  [MyClass2 doItClass6];
  [MyClass2 doItInstance6]; /* { dg-warning "may not respond" } */

}

/*----------------------------------------*/
/* Forward declared root protocols */

@protocol FwProto;

@interface MyClass1 (Forward) <FwProto> /* { dg-warning "definition of protocol .FwProto. not found" } */
@end

Class <FwProto> clsP7 = 0;

void
testForwardeDeclared1(void)
{
  [cls doItClass7];         /* { dg-warning "no .\\+doItClass7. method found" } */
  [cls doItInstance7];      /* { dg-warning "no .\\+doItInstance7. method found" } */

  [clsP7 doItClass7];       /* { dg-warning "not found in protocol" } */
  /* { dg-warning "no .\\+doItClass7. method found" "" { target *-*-* } .-1 } */
  [clsP7 doItInstance7];    /* { dg-warning "not found in protocol" } */
  /* { dg-warning "no .\\+doItInstance7. method found" "" { target *-*-* } .-1 } */

  [MyClass1 doItClass7];    /* { dg-warning "may not respond" } */
  [MyClass1 doItInstance7]; /* { dg-warning "may not respond" } */

  [MyClass2 doItClass7];    /* { dg-warning "may not respond" } */
  [MyClass2 doItInstance7]; /* { dg-warning "may not respond" } */

}

@protocol FwProto
+(void)doItClass7;
-(void)doItInstance7;
@end

void
testForwardeDeclared2(void)
{
  [cls doItClass7];
  [cls doItInstance7];

  [clsP7 doItClass7];    
  [clsP7 doItInstance7]; /* { dg-warning "instead of" }  */

  [MyClass1 doItClass7];
  [MyClass1 doItInstance7];

  [MyClass2 doItClass7];
  [MyClass2 doItInstance7];
}

/*----------------------------------------*/
/* Inherited non root protocols */

@protocol MyProto8
+(void)doItClass8;
-(void)doItInstance8;
@end

@protocol MyProto9 <MyProto8>
+(void)doItClass9;
-(void)doItInstance9;
@end

@interface MyClass1 (InheritedNonRoot) <MyProto9>
@end

Class <MyProto8> clsP8 = 0;
Class <MyProto9> clsP9 = 0;

void
testInheritedNonRoot(void)
{
  [cls doItClass8];
  [cls doItInstance8];
  [cls doItClass9];
  [cls doItInstance9];

  [clsP8 doItClass8];
  [clsP8 doItInstance8]; /* { dg-warning "instead of" }  */
  [clsP8 doItClass9];    /* { dg-warning "not found in protocol" } */
  [clsP8 doItInstance9]; /* { dg-warning "not found in protocol" } */

  [clsP9 doItClass8];
  [clsP9 doItInstance8]; /* { dg-warning "instead of" }  */
  [clsP9 doItClass9];
  [clsP9 doItInstance9]; /* { dg-warning "instead of" }  */

  [MyClass1 doItClass8];
  [MyClass1 doItInstance8];
  [MyClass1 doItClass9];
  [MyClass1 doItInstance9];

  [MyClass2 doItClass8];
  [MyClass2 doItInstance8];
  [MyClass2 doItClass9];
  [MyClass2 doItInstance9];
  
}

/*----------------------------------------*/
/* Prototype mismatch  */

@protocol MyOtherProto1
+(id)doItClass1;
-(id)doItInstance1;
@end
@interface MyOtherClass1 <MyOtherProto1>
@end

Class <MyOtherProto1> oclsP1;

void
testPrototypeMismatch(void)
{
  id tmp1 = [oclsP1 doItClass1];
  id tmp2 = [oclsP1 doItInstance1]; /* { dg-warning "instead of" }  */

  [clsP1 doItClass1];
  [clsP1 doItInstance1]; /* { dg-warning "instead of" }  */
}

id obj = nil;
id <MyProto1> objP1 = nil;
id <MyProto2> objP2 = nil;
id <MyProto5> objP5 = nil;
int num = 0;
void *ptr = 0;

MyClass1 *mc1 = nil;

void
testComptypes(void)
{
  { /* id <protocol>, id <protocol>  */
    objP1 == objP2;  /* { dg-warning "lacks a cast" } */
    objP2 == objP1;  /* { dg-warning "lacks a cast" } */

    objP1 == objP5;
    objP5 == objP1;
  }
  { /* id <protocol>, SomeClass *  */
    mc1 == objP1;
    objP1 == mc1;

    mc1 == objP2; /* { dg-warning "lacks a cast" } */
    objP2 == mc1; /* { dg-warning "lacks a cast" } */
  }
  { /* id <protocol>, id  */
    obj == objP1;
    objP1 == obj;
  }
  { /* id <protocol>, Class  */
    cls == objP1; /* { dg-warning "lacks a cast" } */
    objP1 == cls; /* { dg-warning "lacks a cast" } */
  }
  { /* id <protocol>, non-ObjC  */
    num == objP1; /* { dg-warning "between pointer" } */
    objP1 == num; /* { dg-warning "between pointer" } */

    ptr == objP1;
    objP1 == ptr;
  }
  { /* Class <protocol>, Class <protocol> */
    clsP1 == clsP2; /* { dg-warning "lacks a cast" } */
    clsP2 == clsP1; /* { dg-warning "lacks a cast" } */

    clsP1 == clsP5;
    clsP5 == clsP1;
  }
  { /* Class <protocol>, SomeClass * */
    mc1 == clsP1; /* { dg-warning "lacks a cast" } */
    clsP1 == mc1; /* { dg-warning "lacks a cast" } */
  }
  { /* Class <protocol>, id */
    obj == clsP1;
    clsP1 == obj;
  }
  { /* Class <protocol>, Class */
    cls == clsP1;
    clsP1 == cls;
  }
  { /* Class <protocol>, non-ObjC */
    num == clsP1; /* { dg-warning "between pointer" } */
    clsP1 == num; /* { dg-warning "between pointer" } */

    ptr == clsP1;
    clsP1 == ptr;
  }
  { /* Class <protocol>, id <protocol> */
    clsP1 == objP1; /* { dg-warning "lacks a cast" } */
    objP1 == clsP1; /* { dg-warning "lacks a cast" } */
  }

  { /* id <protocol>, id <protocol>  */
    objP1 = objP2; /* { dg-warning "does not conform" } */
    objP2 = objP1; /* { dg-warning "does not conform" } */

    objP1 = objP5;
    objP5 = objP1; /* { dg-warning "does not conform" } */
  }
  { /* id <protocol>, SomeClass *  */
    mc1 = objP1;
    objP1 = mc1;

    mc1 = objP2; /* { dg-warning "does not conform" } */
    objP2 = mc1; /* { dg-warning "does not implement" } */
  }
  { /* id <protocol>, id  */
    obj = objP1;
    objP1 = obj;
  }
  { /* id <protocol>, Class  */
    cls = objP1; /* { dg-warning "distinct Objective\\-C type" } */
    objP1 = cls; /* { dg-warning "distinct Objective\\-C type" } */
  }
  { /* id <protocol>, non-ObjC  */
    num = objP1; /* { dg-warning "makes integer" } */
    objP1 = num; /* { dg-warning "makes pointer" } */

    ptr = objP1;
    objP1 = ptr;
  }
  { /* Class <protocol>, Class <protocol> */
    clsP1 = clsP2; /* { dg-warning "does not conform" } */
    clsP2 = clsP1; /* { dg-warning "does not conform" } */

    clsP1 = clsP5;
    clsP5 = clsP1; /* { dg-warning "does not conform" } */
  }
  { /* Class <protocol>, SomeClass * */
    /* These combinations should always elicit a warning.  */
    mc1 = clsP1; /* { dg-warning "distinct Objective\\-C type" } */
    clsP1 = mc1; /* { dg-warning "distinct Objective\\-C type" } */
    
    mc1 = clsP2; /* { dg-warning "distinct Objective\\-C type" } */
    clsP2 = mc1; /* { dg-warning "distinct Objective\\-C type" } */
  }
  { /* Class <protocol>, id */
    obj = clsP1;
    clsP1 = obj;
  }
  { /* Class <protocol>, Class */
    cls = clsP1;
    clsP1 = cls;
  }
  { /* Class <protocol>, non-ObjC */
    num = clsP1; /* { dg-warning "makes integer" } */
    clsP1 = num; /* { dg-warning "makes pointer" } */

    ptr = clsP1;
    clsP1 = ptr;
  }
  { /* Class <protocol>, id <protocol> */
    clsP1 = objP1; /* { dg-warning "distinct Objective\\-C type" } */
    objP1 = clsP1; /* { dg-warning "distinct Objective\\-C type" } */
  }
}

int main ()
{
  testSimple();
  testCategory();
  testCategoryInherited();
  return(0);
}

/* { dg-warning "messages without a matching method signature will be assumed to return .id. and accept .\.\.\.. as arguments" "" { target *-*-* } 0 } */
