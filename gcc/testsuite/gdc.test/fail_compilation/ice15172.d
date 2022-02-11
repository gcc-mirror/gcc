/*
TEST_OUTPUT:
---
fail_compilation/ice15172.d(14): Error: constructor `ice15172.ThreadError.this` no match for implicit `super()` call in constructor
---
*/

// 1. ClassDeclaration.semantic
class ThreadError : Error
{
    // 2. FuncDeclaration.semantic
    // 4. FuncDeclaration.semantic3
    //    --> error happens
    this(string)
    {
    }
}

// 3. FuncDeclaration.semantic
// 5. FuncDeclaration.semantic3
void onThreadError()
{
    // 6. NewExp.semantic
    //    --> cd.members.errors == false, cd.members.semantic3Errors == true
    //    BUT, The ThreadError class constructor is not a template function, so
    //    the errors inside its function body won't be associated with the validness of this NewExp.
    //    Therefore, converting the semantic3Error to ErrorExp is not correct.
    // 7. ctfeInterpret
    //    Finally, FuncDeclaration::interpret may encounter a function which is semantic3Errors == true. So
    //    7a. functionSemantic3() should return false if semantic3Errors is true.
    //    7b. the function body errors may not happen during ctfeInterpret call and global.errors could be unincremented.
    __gshared auto ThreadError = new ThreadError(null);
}
