# This makefile is run by the parent dir's makefile.
# thisdir1=`pwd`; \
# srcdir1=`cd $(srcdir); pwd`; \
# cd objc; \
# $(MAKE) $(MAKEFLAGS) -f $$srcdir1/objc/Makefile libobjc.a \
#   srcdir=$$srcdir1 tooldir=$(tooldir) AR="$(AR)" AR_FLAGS="$(AR_FLAGS)" \
#   GCC_FOR_TARGET="$$thisdir1/xgcc -B$$thisdir1/" \
#   GCC_CFLAGS="$(GCC_CFLAGS)"
# Two targets are used by ../Makefile: `all' and `mostlyclean'.

.SUFFIXES: .m

VPATH = $(srcdir)/objc

AR = ar
AR_FLAGS = rc

# Always search these dirs when compiling.
SUBDIR_INCLUDES = -I. -I.. -I$(srcdir) -I$(srcdir)/config

.c.o:
	$(GCC_FOR_TARGET) -c $(GCC_CFLAGS) $(SUBDIR_INCLUDES) $<

.m.o:
	$(GCC_FOR_TARGET) -c $(GCC_CFLAGS) $(SUBDIR_INCLUDES) $<

# If we were not invoked from the parent dir,
# invoke make in the parent dir and have reinvoke this makefile.
# That's necessary to get the right values for srcdir, etc.
all:
	cd ..; $(MAKE) sublibobjc.a

libobjc.a: core.o hash.o object.o
	-rm -f libobjc.a
	$(AR) rc libobjc.a object.o core.o hash.o
# ranlib is run in the parent directory's makefile.

OBJC_H = objc.h objc-proto.h record.h $(srcdir)/assert.h $(srcdir)/gstdarg.h
HASH_H = hash.h mutex.h

core.o: core.c $(OBJC_H) $(HASH_H) $(srcdir)/gstddef.h
hash.o: hash.c $(OBJC_H) $(HASH_H) $(srcdir)/gstddef.h
object.o: object.m object.h $(OBJC_H)
	$(GCC_FOR_TARGET) -c $(GCC_CFLAGS) $(SUBDIR_INCLUDES) $<

mostlyclean:
	-rm -f core.o hash.o object.o libobjc.a
clean: mostlyclean
distclean: mostlyclean
extraclean: mostlyclean
