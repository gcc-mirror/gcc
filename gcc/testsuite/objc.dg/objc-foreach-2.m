/* Syntax check for the new foreach statement. */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

typedef struct objc_class *Class;

typedef struct objc_object {
 Class isa;
} *id;


@interface MyList 
@end

@implementation MyList
- (unsigned int)countByEnumeratingWithState:(struct __objcFastEnumerationState *)state objects:(id *)items count:(unsigned int)stackcount
{
        return 0;
}
- (void)addObject:object {
}

@end

@interface MyList (BasicTest)
- (void)compilerTestAgainst;
@end
void BEGIN();
void INFORLOOP();
void END();
@implementation MyList (BasicTest)
- (void)compilerTestAgainst {

	id elem;
	BEGIN();
	for (elem in (self)) 
	  if (elem)
	    INFORLOOP();
	END();
}
@end

