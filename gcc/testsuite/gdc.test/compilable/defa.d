// PERMUTE_ARGS:
// EXTRA_FILES: imports/defaa.d imports/defab.d imports/defac.d imports/defad.d
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
