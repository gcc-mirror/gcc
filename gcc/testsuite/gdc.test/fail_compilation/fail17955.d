// https://issues.dlang.org/show_bug.cgi?id=17955
/*
TEST_OUTPUT:
---
fail_compilation/fail17955.d(81): Error: cannot create instance of abstract class `SimpleTimeZone`
fail_compilation/fail17955.d(81):        function `bool hasDST()` is not implemented
fail_compilation/fail17955.d(93): Error: template instance `fail17955.SimpleTimeZone.fromISOExtString!dstring` error instantiating
fail_compilation/fail17955.d(25):        instantiated from here: `fromISOExtString!string`
fail_compilation/fail17955.d(56):        instantiated from here: `isISOExtStringSerializable!(SysTime)`
fail_compilation/fail17955.d(49):        instantiated from here: `toRedis!(SysTime)`
fail_compilation/fail17955.d(40):        ... (2 instantiations, -v to show) ...
fail_compilation/fail17955.d(32):        instantiated from here: `indicesOf!(isRedisType, resetCodeExpireTime)`
fail_compilation/fail17955.d(67):        instantiated from here: `RedisStripped!(User, true)`
fail_compilation/fail17955.d(93): Error: need `this` for `fromISOExtString` of type `pure nothrow @nogc @safe immutable(SimpleTimeZone)(dstring _param_0)`
fail_compilation/fail17955.d(95): Error: undefined identifier `DateTimeException`
fail_compilation/fail17955.d(25): Error: variable `fail17955.isISOExtStringSerializable!(SysTime).isISOExtStringSerializable` - type `void` is inferred from initializer `fromISOExtString("")`, and variables cannot be of type `void`
fail_compilation/fail17955.d(54): Error: function `fail17955.toRedis!(SysTime).toRedis` has no `return` statement, but is expected to return a value of type `string`
---
*/

alias Alias(alias a) = a;

template isISOExtStringSerializable(T)
{
    enum isISOExtStringSerializable = T.fromISOExtString("");
}

template RedisObjectCollection(){}

struct RedisStripped(T, bool strip_id = true)
{
    alias unstrippedMemberIndices = indicesOf!(Select!(strip_id,
            isRedisTypeAndNotID, isRedisType), T.tupleof);
}

template indicesOf(alias PRED, T...)
{
    template impl(size_t i)
    {
        static if (PRED!T)
            impl TypeTuple;
    }

    alias indicesOf = impl!0;
}

template isRedisType(alias F)
{
    enum isRedisType = toRedis!(typeof(F));
}

template isRedisTypeAndNotID(){}

string toRedis(T)()
{
    static if (isISOExtStringSerializable!T)
        return;
}

struct User
{
    SysTime resetCodeExpireTime;
}

class RedisUserManController
{
    RedisObjectCollection!(RedisStripped!User) m_users;
}

class TimeZone
{
    abstract bool hasDST();
}

class SimpleTimeZone : TimeZone
{
    unittest {}

    immutable(SimpleTimeZone) fromISOExtString(S)(S)
    {
        new SimpleTimeZone;
    }
}

struct SysTime
{

    static fromISOExtString(S)(S)
    {
        dstring zoneStr;

        try
            SimpleTimeZone.fromISOExtString(zoneStr);

        catch (DateTimeException e) {}
    }
}

template Select(bool condition, T...)
{
    alias Select = Alias!(T[condition]);
}
