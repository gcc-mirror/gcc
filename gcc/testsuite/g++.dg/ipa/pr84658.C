/* PR ipa/84658 */
/* { dg-do run } */
/* { dg-options "-O2 -fmerge-all-constants -std=c++11" } */

const int kTestCasesFoo[] = { 0, 1, 2, 3, 4, 5, 8, 15, 16, 17, 512, 1020, 1021, 1022, 1023, 1024 };
const int kTestCasesBar[] = { 0, 1, 2, 3, 4, 5, 8, 15, 16, 17, 512, 1020, 1021, 1022, 1023, 1024 };

void Foo() {
    __builtin_printf("foo:");
    for (int count : kTestCasesFoo) {
        __builtin_printf("%d,", count);
    }
    __builtin_printf(";\n");
}

void Bar() {
    __builtin_printf("bar:");
    for (int count : kTestCasesBar) {
        __builtin_printf("%d,", count);
    }
    __builtin_printf(";\n");
}

int main() {
    Foo();
    Bar();
}

/* { dg-output "foo:0,1,2,3,4,5,8,15,16,17,512,1020,1021,1022,1023,1024,;(\n|\n\r|\r)*" } */
/* { dg-output "bar:0,1,2,3,4,5,8,15,16,17,512,1020,1021,1022,1023,1024,;(\n|\n\r|\r)*" } */
