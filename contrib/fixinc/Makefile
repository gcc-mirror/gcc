
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##
##  Makefile for constructing the "best" include fixer we can
##
##  $Id: Makefile,v 1.4 1998/08/05 10:20:10 korbb Exp $
##
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

OBJ = fixincl.o server.o regex.o
HDR = server.h regex.h
## LIB = -lgen
SH_TARGET  = inclhack.sh fixincl.sh
BIN_TARGET = fixincl
AG_TEXT    = $(SH_TARGET) fixincl.x \
	inclhack.def inclhack.tpl hackshell.tpl fixincl.tpl
TARGETS    = $(SH_TARGET) $(BIN_TARGET)

default : gen

all : $(TARGETS)
sh  : $(SH_TARGET)
gen : $(SH_TARGET) fixincl.x

$(OBJ): $(HDR)

fixincl: $(OBJ)
	@echo $(CC) -o $@ $(OBJ) $(LIB) ; \
	if $(CC) -o $@ $(OBJ) $(LIB) ; then : ; else \
	rm -f $@ ; (echo "#! /bin/sh" ; echo exit 1 ) > $@ ; \
	chmod 777 $@ ; fi

regex.o: regex.c
	-$(CC) -g -DSTDC_HEADERS=1 -c regex.c

fixincl.o : fixincl.x fixincl.c
	-$(CC) $(CFLAGS) -o $@ -c fixincl.c

server.o : server.c server.h
	-$(CC) $(CFLAGS) -o $@ -c server.c

fixincl.x: fixincl.tpl inclhack.def
	@if ( autogen --help > /dev/null 2>&1 ) ; then \
		echo autogen -T fixincl.tpl -b fixincl inclhack.def ; \
		autogen -T fixincl.tpl -b fixincl inclhack.def ; \
	else echo You need to install autogen ; touch $@ ; fi

inclhack.sh: inclhack.def inclhack.tpl hackshell.tpl
	@if ( autogen --help > /dev/null 2>&1 ) ; then \
		echo autogen inclhack.def ; \
		autogen inclhack.def ; \
	else echo You need to install autogen ; touch $@ ; fi

fixincl.sh: inclhack.def inclhack.tpl
	@if ( autogen --help > /dev/null 2>&1 ) ; then \
		echo autogen -DPROGRAM=1 -b fixincl inclhack.def ; \
		autogen -DPROGRAM=1 -b fixincl inclhack.def ; touch $@ ; \
	else echo You need to install autogen ; touch $@ ; fi ; \

clean:
	rm -f *.o $(TARGETS) fixincl.x

#  Ensure all the targets are built.  If the program "fixincl"
#  failed to compile, link or load, then we install the
#  "inclhack.sh" script.  Otherwise, we install that program
#  plus the wrapper script, "fixincl.sh".
#
install: $(TARGETS)
	@rm -f $(DESTDIR)/fixinc.sh ; \
	if ( ./fixincl -v > /dev/null 2>&1 ) ; then \
		echo cp fixincl.sh $(DESTDIR)/fixinc.sh ; \
		cp fixincl.sh $(DESTDIR)/fixinc.sh ; \
		chmod 555 $(DESTDIR)/fixinc.sh ; \
    	rm -f $(DESTDIR)/fixincl ; \
		echo cp fixincl $(DESTDIR) ; \
		cp fixincl $(DESTDIR) ; \
		chmod 555 $(DESTDIR)/fixincl ; \
	else \
		echo cp inclhack.sh $(DESTDIR)/fixinc.sh ; \
		cp inclhack.sh $(DESTDIR)/fixinc.sh ; \
	fi
