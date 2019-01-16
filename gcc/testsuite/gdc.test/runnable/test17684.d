struct StructField(T)
{
    static T Field;
    static alias Field this;
}

struct StructProperty(T)
{
    static T Field;
	
	static @property T property()
	{
		return Field;	
	}
	
	static @property void property(T value)
	{
		Field = value;	
	}
	
    static alias property this;
}

class ClassField(T)
{
    static T Field;
    static alias Field this;
}

class ClassProperty(T)
{
    static T Field;
	
	static @property T property()
	{
		return Field;	
	}
	
	static @property void property(T value)
	{
		Field = value;	
	}
	
    static alias property this;
}

bool boolTest(T)()
{
    alias t = T;

    t = false;                    // tests AssignExp
    assert(t == false);

    bool boolValue = t;           // tests AssignExp
    assert(boolValue == false);

    t = !t;                       // tests NotExp
    assert(t == true);

    boolValue = t;
    assert(boolValue == true);

    assert(boolValue && t);       // tests AndAndExp
    assert(t && boolValue);

    boolValue = false;
    assert(boolValue || t);       // tests OrOrExp
    assert(t || boolValue);

    assert(t != boolValue);       // tests CmpExp
    assert(boolValue != t);

    boolValue = true;
    assert(t == boolValue);
    assert(boolValue == t);

    t = true;
    return t;                     // tests ReturnStatement
}

int intTest(T)()
{
    alias t = T;

    t = 42;                       // tests AssignExp
    assert(t == 42);

    int intValue = t;
    assert(intValue == 42);

    assert(t == 42);              // tests CmpExp
    assert(42 == t);
    assert(t != 43);
    assert(43 != t);
    assert(t < 43);
    assert(43 > t);
    assert(t <= 42);
    assert(42 >= t);

    // These currently don't work for properties due to https://issues.dlang.org/show_bug.cgi?id=8006
    static if (!(typeid(T) is typeid(StructProperty!int)) && !(typeid(T) is typeid(ClassProperty!int)))
    {
        t++;              // test a few unary and binary operators
        assert(t == 43);

        t += 1;
        assert(t == 44);

        t--;
        assert(t == 43);

        t -= 1;
        assert(t == 42);
    }

    assert(~t == ~42);            // tests ComExp

    return t;                     // tests ReturnStatement
}

void main()
{
    assert(boolTest!(StructField!(bool))());
    assert(boolTest!(StructProperty!(bool))());
    assert(boolTest!(ClassField!(bool))());
    assert(boolTest!(ClassProperty!(bool))());

    assert(intTest!(StructField!(int))() == 42);
    assert(intTest!(StructProperty!(int))() == 42);
    assert(intTest!(ClassField!(int))() == 42);
    assert(intTest!(ClassProperty!(int))() == 42);
}
