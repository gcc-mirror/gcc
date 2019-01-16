// REQUIRED_ARGS: -O -inline -c


final class SharedLib {
    void getSymbol() {return getSymbolImpl();}
    void getSymbolImpl() {return getSymbol_();}
    /* add more intermediate functions to go slower */
    void getSymbol_() {}
}


void test13193()
{
SharedLib ssllib;
void bindFunc() {ssllib.getSymbol();}
    bindFunc(); /* add more of these to go slower */
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 10 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 20 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 30 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 40 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 50 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 60 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 70 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 80 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 90 */

    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc();
    bindFunc(); /* 100 */
}

