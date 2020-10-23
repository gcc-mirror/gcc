module imports.test21299.rootstringtable;
struct StringValue(T)
{
    char* lstring()
    {
        return cast(char*)&this;
    }
}

struct StringTable(T)
{
    StringValue!T* insert()
    {
        allocValue;
        return getValue;
    }

    uint allocValue()
    {
        StringValue!(T) sv;
        sv.lstring[0] = 0;
        return 0;
    }

    StringValue!T* getValue()
    {
        return cast(StringValue!T*)&this;
    }
}

// Other tests are the same as the original issue, but use other kinds of
// nesting Dsymbols that need to be handled by templateInstanceSemantic().
struct StringValue2(T)
{
    char* lstring()
    {
        return cast(char*)&this;
    }
}

struct StringTable2(T)
{
  @nogc // AttribDeclaration (also covers pragma, extern(), static foreach, ...)
  {
    StringValue2!T* insert()
    {
        allocValue;
        return getValue;
    }

    uint allocValue()
    {
        StringValue2!(T) sv;
        sv.lstring[0] = 0;
        return 0;
    }

    StringValue2!T* getValue()
    {
        return cast(StringValue2!T*)&this;
    }
  }
}

//
struct StringValue3(T)
{
    char* lstring()
    {
        return cast(char*)&this;
    }
}

struct StringTable3(T)
{
  static if (true) // ConditionalDeclaration (static if)
  {
    StringValue3!T* insert()
    {
        allocValue;
        return getValue;
    }

    uint allocValue()
    {
        StringValue3!(T) sv;
        sv.lstring[0] = 0;
        return 0;
    }

    StringValue3!T* getValue()
    {
        return cast(StringValue3!T*)&this;
    }
  }
}
