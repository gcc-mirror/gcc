module imports.a18a;

interface IEnumerator
{
}

class Enumerator(T) : IEnumerator
{
	this()
	{
	}
}

interface IContainer(T)
{
	alias   Container!(int)		selected_type;

	IEnumerator	enumerate();
}

class Container(T) : IContainer!(int)
{
	IEnumerator	enumerate()
	{
	    return new Enumerator!(int)();
	}
}

