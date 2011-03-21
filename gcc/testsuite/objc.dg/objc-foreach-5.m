/* FIXME: Run this test with the GNU runtime as well.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */

#import <Foundation/Foundation.h>

NSArray * createTestVictim(unsigned capacity) {
    NSMutableArray * arr = [[NSMutableArray alloc] initWithCapacity:capacity];
    int x = 0;

    for(x = 0; x < capacity; x++) {
        NSNumber * num = [NSNumber numberWithInteger:x];
        [arr addObject:num];
    }
    
    NSArray * immutableCopy = [arr copy];
    [arr release];
    
    return immutableCopy;
}

void addStuffUp(NSArray * values) {
    NSInteger accumulator = 0;
//    for (id item in values) {
    id item;
    for (item in values) {
        accumulator += [item integerValue];
    }
}

int main (int argc, char const* argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    NSArray * target = createTestVictim(10);
    addStuffUp(target);
    [pool release];
    return 0;
}
/* { dg-final { scan-assembler "_addStuffUp:" } } */
