# Build the include directory.  The stamp files are stmp-* rather than
# stamp-* so that mostlyclean does not force the include directory to
# be rebuilt.


# Copy in the headers provided with gcc.
USER_H = $(srcdir)\ginclude\stdarg.h $(srcdir)\ginclude\stddef.h \
    $(srcdir)\ginclude\varargs.h $(srcdir)\ginclude\va-alpha.h \
    $(srcdir)\ginclude\va-h8300.h $(srcdir)\ginclude\va-i860.h \
    $(srcdir)\ginclude\va-i960.h $(srcdir)\ginclude\va-mips.h \
    $(srcdir)\ginclude\va-m88k.h $(srcdir)\ginclude\va-pa.h \
    $(srcdir)\ginclude\va-pyr.h $(srcdir)\ginclude\va-sparc.h \
    $(srcdir)\ginclude\va-clipper.h $(srcdir)\ginclude\va-spur.h \
    $(srcdir)\ginclude\iso646.h \
    $(srcdir)\ginclude\proto.h

# Build the include directory except for float.h (which depends upon
# enquire).

stmp-int-hdrs: $(USER_H)
	type $(srcdir)\limitx.h >xlimits.h
	type $(srcdir)\glimits.h >>xlimits.h
	type $(srcdir)\limity.h >>xlimits.h

	-mkdir include	
	for %%f in ($(USER_H)) do copy %%f include
	del include\limits.h
	copy xlimits.h include\limits.h
	del include\syslimits.h
	copy $(srcdir)\gsyslimits.h include\syslimits.h
	copy include\limits.h include\syslimits.h
	del include\README
	copy $(srcdir)\README-fixinc include\README
	touch stmp-int-hdrs

stmp-headers: stmp-int-hdrs fixinc-nt.exe
	fixinc-nt
	touch stmp-headers

# Build float.h.
stmp-float_h: libgcc.lib enquire.exe
	-.\enquire -f > tmp-float.h
	del include\float.h
	copy tmp-float.h include\float.h
	touch stmp-float_h

fixinc-nt.obj: $(srcdir)/config/winnt/fixinc-nt.c
	cl -c -I. -I$(srcdir) -I$(srcdir)/include -I$(srcdir)/config/winnt $(srcdir)/config/winnt/fixinc-nt.c

fixinc-nt.exe: fixinc-nt.obj dirent.obj
	cl fixinc-nt.obj dirent.obj libc.lib kernel32.lib
