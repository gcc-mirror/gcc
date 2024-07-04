/* REQUIRED_ARGS: -preview=dip1000 -Ifail_compilation/imports
TEST_OUTPUT:
---
fail_compilation/issue21685_main.d(12): Error: constructor `issue21685.E.this` is not accessible from module `issue21685_main`
fail_compilation/issue21685_main.d(19): Error: constructor `issue21685.E.this` is not accessible from module `issue21685_main`
---
*/
import issue21685;

void main()
{
    new E;
}

class F : E
{
    this()
    {
        super();
    }
}
