// PERMUTE_ARGS:

struct A(T) { ~this() {} }
class C { A!int[1] array; }

void test14838() pure nothrow @nogc @safe
{
    C c;
    c.__xdtor();    // C.~this() will also be inferred to
                    // pure nothrow @nogc @safe

    A!int[1] array;
    // scope destructor call does not cause attribute violation.
}

// ----

/*
 * This is a reduced test case comes from std.container.Array template,
 * to fix the semantic analysis order issue for correct destructor attribute inference.
 *
 * Before the bugfix:
 *   1. StructDeclaration('Array!int').semantic() instantiates
 *      RangeT!(Array!int) at the `alias Range = ...;`, but
 *      StructDeclaration('RangeT!(Array!int)').semantic() exits
 *      with sizeok == SIZEOKfwd, because the size of _outer_ field is not yet determined.
 *   2. StructDeclaration('Array!int').semantic() succeeds to determine the size
 *      (sizeok = SIZEOKdone).
 *   3. StructDeclaration('Array!int').buildOpAssign() will generate opAssign because
 *      Array!int._data field has identity opAssign member function.
 *   4. The semantic3 will get called for the generated opAssign, then
 *         6-1. Array!int.~this() semantic3, and
 *         6-2. RefCounted!(Array!int.Payload).~this() semantic3
 *      will also get called to infer their attributes.
 *   5. In RefCounted!(Array!int.Payload).~this(), destroy(t) will be instantiated.
 *      At that, TemplateInstance.expandMembers() will invoke runDeferredSemantic()
 *      and it will re-run StructDeclaration('RangeT!(Array!int)').semantic().
 *   6. StructDeclaration('RangeT!(Array!int)').semantic() determines the size
 *      (sizeok = SIZEOKdone). Then, it will generate identity opAssign and run its semantic3.
 *      It will need to infer RangeT!(Array!int).~this() attribute, then it requires the
 *      correct attribute of Array!int.~this().
 *
 *      However, the Array!int.~this() attribute is not yet determined! [bug]
 *      -> it's wongly handled as impure/system/throwable/gc-able.
 *
 *      -> then, the attribute inference results for
 *         RangeT!(Array!int).~this() and Array!int.~this() will be incorrect.
 *
 * After the bugfix:
 *   In 6, StructDeclaration('RangeT!(Array!int)').semantic() will check that:
 *   all base struct types of the instance fields have completed addition of
 *   special functions (dtor, opAssign, etc).
 *   If not, it will defer the completion of its semantic pass.
 */

void destroy14838(S)(ref S s) if (is(S == struct))
{
    s.__xdtor();
}

struct RefCounted14838(T)
{
    ~this()
    {
        T t;
        .destroy14838(t);
    }

    void opAssign(typeof(this) rhs) {}
}

struct RangeT14838(A)
{
    A[1] _outer_;
}

struct Array14838(T)
{
    struct Payload
    {
        ~this() {}
    }
    RefCounted14838!Payload _data;

    alias Range = RangeT14838!Array14838;
}

class Test14838
{
    Array14838!int[1] field;
}
