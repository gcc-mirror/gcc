// PERMUTE_ARGS:

module defa;

private import imports.defaa;
	
public abstract class A
{
	Display d;
	int style;

	this() {}

	public this(A parent, int style)
	{
		this.style = style;
		d = parent.d;
	}
}
