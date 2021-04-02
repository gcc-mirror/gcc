/*
REQUIRED_ARGS: -Irunnable/imports
COMPILED_IMPORTS: imports/test18322import.d
PERMUTE_ARGS:
*/
import test18322import;
void main(){
    version(Windows)
        auto sep = "\\";
    else
        auto sep = "/";

    auto filename = "runnable" ~ sep ~ "test18322.d";

    fun(filename);
    mixin(`fun(filename ~ "-mixin-16");`);

    #line 100 "poundlinefile.d"
    fun("poundlinefile.d");
    mixin(`fun("poundlinefile.d-mixin-101");`);
}
