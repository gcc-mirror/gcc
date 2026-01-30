// https://github.com/dlang/dmd/issues/20334
// EXTRA_FILES: imports/imp20344.c
import imports.imp20344;
string s = X;
const(char)* p = X;

void takes_string(string){ }
void takes_char_star(const(char)*){ }


void test20344(){
    takes_string(X);
    takes_char_star(X);
}
