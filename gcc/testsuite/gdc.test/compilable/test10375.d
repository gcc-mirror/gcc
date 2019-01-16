// REQUIRED_ARGS: -o-

import imports.test10375a;

void packIt(Pack)(Pack p){ }  //3

void main()
{
    alias p = packIt!(int);
    p(2);               // OK <- NG
    packIt(2);          // OK <- NG
    packIt!(int)(2);    // OK <- NG
}
