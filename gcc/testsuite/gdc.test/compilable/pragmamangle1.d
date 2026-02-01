// Tests for pragma mangle
module pragmamangle;

version (Posix): // Itanium C++ ABI only

string ctfe(string s) { return "mangle_" ~ "ctfe_" ~ s; }
immutable string const_function = "mangle_const_function";
immutable string const_variable = "mangle_const_variable";

// This mangle string doesn't propagate to tests that use type_symbol.
pragma(mangle, "Q_should_mangle_string_propagate")
struct type_symbol { }

class template_symbol(T) { }

/* 1. Overrides the default mangling for a symbol.
 */
pragma(mangle, "mangle_function") void test_fun1();
static assert(test_fun1.mangleof == "mangle_function");

pragma(mangle, "mangle_attribute") extern(C) { nothrow { void test_pokeattr1(); } }
static assert(test_pokeattr1.mangleof == "mangle_attribute");

/* 2a. For variables and functions there must be one AssignExpression and it
 * must evaluate at compile time to a string literal
 */
pragma(mangle, ctfe("function")) void test_fun2a1();
static assert(test_fun2a1.mangleof == "mangle_ctfe_function");

pragma(mangle, const_function) void test_fun2a2();
static assert(test_fun2a2.mangleof == "mangle_const_function");

pragma(mangle, ctfe("variable")) int test_var2a1;
static assert(test_var2a1.mangleof == "mangle_ctfe_variable");

pragma(mangle, const_variable) int test_var2a2;
static assert(test_var2a2.mangleof == "mangle_const_variable");

/* 2b. For aggregates there may be one or two AssignExpressions, one of which
 * must evaluate at compile time to a string literal and one which must
 * evaluate to a symbol.
 * [UNDOCUMENTED] The pragma(mangle) attribute only gets applied to the
 * encoded parameters types of extern(C++) functions.
 */
pragma(mangle, "mangle_struct") extern(C++) { struct S1 { } }
extern(C++) void test_struct2b1(S1);
extern(D) void externD_struct2b1(S1);
static assert(test_struct2b1.mangleof == "_Z14test_struct2b113mangle_struct");
static assert(externD_struct2b1.mangleof == "_D12pragmamangle17externD_struct2b1FSQBj2S1Zv");

pragma(mangle, type_symbol, "mangle_struct") struct S2 { }
extern(C++) void test_struct2b3(S2);
extern(D) void externD_struct2b3(S2);
static assert(test_struct2b3.mangleof == "_Z14test_struct2b313mangle_struct");
static assert(externD_struct2b3.mangleof == "_D12pragmamangle17externD_struct2b3FSQBj2S2Zv");

pragma(mangle, "mangle_struct", type_symbol) struct S3 { }
extern(C++) void test_struct2b4(S3);
extern(D) void externD_struct2b4(S3);
static assert(test_struct2b4.mangleof == "_Z14test_struct2b413mangle_struct");
static assert(externD_struct2b4.mangleof == "_D12pragmamangle17externD_struct2b4FSQBj2S3Zv");

// Repeat struct tests on classes.

pragma(mangle, "mangle_class") extern(C++) { class C1 { } }
extern(C++) void test_class2b1(C1);
extern(D) void externD_class2b1(C1);
static assert(test_class2b1.mangleof == "_Z13test_class2b1P12mangle_class");
static assert(externD_class2b1.mangleof == "_D12pragmamangle16externD_class2b1FCQBi2C1Zv");

pragma(mangle, type_symbol, "mangle_class") class C2 { }
extern(C++) void test_class2b3(C2);
extern(D) void externD_class2b3(C2);
static assert(test_class2b3.mangleof == "_Z13test_class2b3P12mangle_class");
static assert(externD_class2b3.mangleof == "_D12pragmamangle16externD_class2b3FCQBi2C2Zv");

pragma(mangle, "mangle_class", type_symbol) class C3 { }
extern(C++) void test_class2b4(C3);
extern(D) void externD_class2b4(C3);
static assert(test_class2b4.mangleof == "_Z13test_class2b4P12mangle_class");
static assert(externD_class2b4.mangleof == "_D12pragmamangle16externD_class2b4FCQBi2C3Zv");

/* 2c. If that symbol is a TemplateInstance, the aggregate is treated as a
 * template that has the signature and arguments of the TemplateInstance.
 */
template T1(C)
{
    pragma(mangle, C, "mangle_template") struct T1 { }
}
extern(C++) void test_template2c3(T1!(template_symbol!int));
extern(C++) void test_template2c4(T1!(template_symbol!float));
extern(C++) void test_template2c5(T1!(type_symbol));
static assert(test_template2c3.mangleof == "_Z16test_template2c315mangle_templateIiE");
static assert(test_template2c4.mangleof == "_Z16test_template2c415mangle_templateIfE");
static assert(test_template2c5.mangleof == "_Z16test_template2c515mangle_template");

template T2(C)
{
    pragma(mangle, "mangle_template", C) struct T2 { }
}
extern(C++) void test_template2c6(T2!(template_symbol!int));
extern(C++) void test_template2c7(T2!(template_symbol!float));
extern(C++) void test_template2c8(T2!(type_symbol));
static assert(test_template2c6.mangleof == "_Z16test_template2c615mangle_templateIiE");
static assert(test_template2c7.mangleof == "_Z16test_template2c715mangle_templateIfE");
static assert(test_template2c8.mangleof == "_Z16test_template2c815mangle_template");

/* 2d. The identifier of the symbol is used when no string is supplied.
 */
pragma(mangle, type_symbol) struct I1 { }
extern(C++) void test_struct2d1(I1);
static assert(test_struct2d1.mangleof == "_Z14test_struct2d111type_symbol");

pragma(mangle, type_symbol) class I2 { }
extern(C++) void test_class2d1(I2);
static assert(test_class2d1.mangleof == "_Z13test_class2d1P11type_symbol");

template I3(C)
{
    pragma(mangle, C) struct I3 { }
}
extern(C++) void test_template2d1(I3!(template_symbol!int));
extern(C++) void test_template2d2(I3!(template_symbol!float));
extern(C++) void test_template2d3(I3!(type_symbol));
static assert(test_template2d1.mangleof == "_Z16test_template2d115template_symbolIiE");
static assert(test_template2d2.mangleof == "_Z16test_template2d215template_symbolIfE");
static assert(test_template2d3.mangleof == "_Z16test_template2d311type_symbol");

// ??? No template arguments encoded.

pragma(mangle, template_symbol!float) struct I4 { }
extern(C++) void test_template2d4(I4);
static assert(test_template2d4.mangleof == "_Z16test_template2d415template_symbol");

/* 3. It only applies to function and variable symbols. Other symbols are
 * ignored.
 */
pragma(mangle, "mangle_alias")
alias ignored_alias = int;

pragma(mangle, "mangle_enum")
enum ignored_enum { a = 1 }

pragma(mangle, "mangle_mixed_ignored")
{
    enum test_ignored1 { b }
    void test_not_ignored();
    alias test_ignored2 = int delegate(int);
}
static assert(test_not_ignored.mangleof == "mangle_mixed_ignored");
