/* REQUIRED_ARGS: -preview=dip1000 -Ifail_compilation/imports
TEST_OUTPUT:
---
fail_compilation/issue21685_main.d(11): Error: class `issue21685.E` constructor `this` is not accessible
---
*/
import issue21685;

void main()
{
    new E;
}
