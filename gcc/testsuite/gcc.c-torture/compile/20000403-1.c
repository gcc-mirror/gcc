struct utsname {
	char	sysname[32 ];	 
	char	version[32 ];	 
};
int
uname(name)
	struct utsname *name;
{
	int mib[2], rval;
	long len;
	char *p;
	int oerrno;
	if (sysctl(mib, 2, &name->sysname, &len, 0 , 0) == -1)
	  ;
	for (p = name->version; len--; ++p) {
				*p = ' ';
	}
}
