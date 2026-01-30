/*
TEST_OUTPUT:
---
fail_compilation/test21429.d(20): Error: no property `z` for `s` of type `test21429.S`
fail_compilation/test21429.d(13):        struct `S` defined here
---
*/
//make sure this fails properly after fixing bug 21429.d
template mixinOpDispatch(string id){
    string opDispatch(string s)() if(s == id){ return id; }
}

struct S {
    mixin mixinOpDispatch!"x";
    mixin mixinOpDispatch!"y";
}

void main(){
    S s;
    auto fail = s.z;
}
