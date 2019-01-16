module imports.test15117a;

struct AssertResult {}

auto test_usr_1()
{
    // 2. generate TyepInfoStructDeclaration
    auto x = typeid(AssertResult);
}
