/* REQUIRED_ARGS: -preview=dip1000 -Ifail_compilation/imports
TEST_OUTPUT:
---
fail_compilation/issue21936.d(15): Error: struct `issue21936s.S` variable `field` is not accessible from `@safe` code
fail_compilation/issue21936.d(15): Error: struct `issue21936s.S` variable `field` is not accessible from `@safe` code
fail_compilation/issue21936.d(11): Error: template instance `issue21936.constructImplicit!(S)` error instantiating
fail_compilation/issue21936.d(7):        instantiated from here: `registerConstructors!(S)`
fail_compilation/issue21936.d(21):        instantiated from here: `registerType!(S)`
---
*/
#line 2
module issue21936;
import issue21936s;
struct Handlers {
    void registerType(T)()
    {
        registerConstructors!T;
    }
    void registerConstructors(T)()
    {
        constructImplicit!T;
    }
}

auto constructImplicit(T)(typeof(T.init.tupleof) x = T.init.tupleof)
{
}

void registerHandlersDateTime(Handlers handlers)
{
	handlers.registerType!(S);
}
