# Build libgcc.a

libgcc.lib : libgcc1.c libgcc2.c mklibgcc.exe
	mklibgcc -c
	mklibgcc "cl -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES)" libgcc1.c $(LIB1FUNCS)
	mklibgcc "xgcc -B./ -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES)" libgcc2.c $(LIB2FUNCS)
	mklibnow.bat
	-del libgcc.lib
	lib -verbose -out:libgcc.lib lgcctmp/*.obj

mklibgcc.obj : $(srcdir)/config/winnt/mklibgcc.c
	cl -I. -I$(srcdir) -I$(srcdir)/config/winnt -c $(srcdir)/config/winnt/mklibgcc.c

dirent.obj : $(srcdir)/config/winnt/dirent.c stmp-int-hdrs
	cl -I. -I$(srcdir) -I$(srcdir)/include -I$(srcdir)/config/winnt -c $(srcdir)/config/winnt/dirent.c

mklibgcc.exe : mklibgcc.obj dirent.obj
	cl mklibgcc.obj dirent.obj libc.lib kernel32.lib

