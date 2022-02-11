// PERMUTE_ARGS:
// EXTRA_FILES: imports/test1imp.d
class File
{
    import imports.test1imp;

    static char[] read(char[] name)
    {
	DWORD size;	// DWORD is defined in test1imp
	return null;
    }

}
