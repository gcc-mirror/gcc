// { dg-do compile }

import gcc.attributes;

@attribute("register", null) int var1; // { dg-error "attribute argument not a string constant" }

@attribute("register", "") int var2; // { dg-error "register name not specified for .var2." }

@attribute("register", "invalid") __gshared int var3; // { dg-error "invalid register name for .var3." }

void f1(ref int r) { }

void test1()
{
    @register("ref") int var6;
    f1(var6); // { dg-error "address of explicit register variable .var6. requested" }
}

void f2(out int r) { }

void test2()
{
    @register("out") int var7;
    f2(var7); // { dg-error "address of explicit register variable .var7. requested" }
}

void f3(lazy int r) { }

void test3()
{
    @register("lazy") int var8; // { dg-error "explicit register variable .var8. cannot be used in nested function" }
    f3(var8);
}

void test4()
{
    @register("addr") int var9;
    auto ptr3 = &var9; // { dg-error "address of explicit register variable .var9. requested" }
}

ref int test5()
{
    @register("refreturn") __gshared int var10; // { dg-error "invalid register name" }
    return var10; // { dg-error "address of explicit register variable .var10. requested" }
}

auto test6()
{
    @register("closure") int var11; // { dg-error "explicit register variable .var11. cannot be used in nested function" }
    int nested()
    {
        return var11;
    }
    return &nested;
}
