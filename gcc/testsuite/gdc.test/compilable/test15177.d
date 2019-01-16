// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:
// EXTRA_SOURCES: imports/test15117a.d

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
