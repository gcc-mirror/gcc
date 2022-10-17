// REQUIRED_ARGS: -o-
// EXTRA_FILES: imports/test10375a.d
import imports.test10375a;

void packIt(Pack)(Pack p){ }  //3

void main()
{
    alias p = packIt!(int);
    p(2);               // OK <- NG
    packIt(2);          // OK <- NG
    packIt!(int)(2);    // OK <- NG
}
