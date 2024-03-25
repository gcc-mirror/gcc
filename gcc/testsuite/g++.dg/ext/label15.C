// PR c++/37722
// { dg-do compile { target indirect_jumps } }
// { dg-options "" }

extern "C" int printf (const char *, ...);
template<int i>
struct foo {
    foo() { printf("%s<%d>\n", __FUNCTION__, i); }
    ~foo() { printf("%s<%d>\n", __FUNCTION__, i); }
};
enum {RETRY, INSIDE, OUTSIDE, EVIL};
int bar(int idx) {
    static void* const gotos[] = {&&RETRY, &&INSIDE, &&OUTSIDE, &&EVIL};
    bool first = true;
    {
    RETRY:			// { dg-warning "jump" }
        foo<1> f1;		// { dg-message "does not destroy" }
        if(first) {
            first = false;
            goto *gotos[idx];	// { dg-message "computed goto" }
        }
    INSIDE:			// OK
        return 1;
    }
    if(0) {
        foo<2> f2;		// { dg-message "crosses initialization" }
    EVIL:			// { dg-warning "jump" }
        return 2;
    }
 OUTSIDE:			// { dg-warning "jump" }
    return 0;
}
int main() {
    for(int i=RETRY; i <= EVIL; i++)
        printf("%d\n", bar(i));
    return 0;
}
