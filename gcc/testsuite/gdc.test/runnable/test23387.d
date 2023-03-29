/* COMPILE_SEPARATELY:
 * EXTRA_SOURCES: imports/maker.i imports/freer.i
 */

// https://issues.dlang.org/show_bug.cgi?id=23387

/+ maker.i
typedef struct Foo *FooRef;
struct Foo {
    int x;
};
FooRef make_foo(void);
+/
import imports.maker;


/+ freer.i
typedef struct Foo *FooRef;
struct Foo {
    int x;
};
void free_foo(FooRef foo);
+/
import imports.freer;

int main(){
    FooRef f = make_foo();
    free_foo(f);
    return 0;
}
