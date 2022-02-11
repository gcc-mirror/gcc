// REQUIRED_ARGS: -o-
// COMPILED_IMPORTS: imports/test15117a.d
/*
TEST_OUTPUT:
---
fail_compilation/test15177.d-mixin-19(19): Error: `imports.test15117a.object` is not visible from module `test15177`
fail_compilation/test15177.d(28): Error: template instance `test15177.RunApiTest!()` error instantiating
---
*/

import users = imports.test15117a;

void RunApiTest(T...)()
{
    foreach (name; __traits(allMembers, users))
    {
        // 3. list the name of TyepInfoStructDeclaration,
        //    but it's just internal symbol and invisible.
        mixin("alias func = users . " ~ name ~ ";");
    }
}

void main()
{
    // 1. run semantic3 of users.test_usr_1
    users.test_usr_1();

    RunApiTest!();
}
