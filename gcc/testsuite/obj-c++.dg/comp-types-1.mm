/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

@interface A
+ new;
@end

@interface B : A
@end

int main(int argc, char **argv) {
    B *b = [B new];
    A *a = b;

    return (b == a);
}

