/* { dg-do compile } */
@class Base;
@protocol _Protocol;

@interface ClassA {
}
-(void) func1:(Base<_Protocol> *)inTarget;
@end

int main()
{
	ClassA* theA = 0;
	Base<_Protocol>* myBase = 0;
	[theA func1:myBase];

	return 0;
}

