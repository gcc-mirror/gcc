
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##
##  Makefile for constructing the "best" include fixer we can
##
##  $Id: Makefile,v 1.1 1998/03/20 16:19:41 korbb Exp $
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

default : fixincl.sh

all : $(TARGETS)
sh  : $(SH_TARGET)

$(OBJ): $(HDR)
fixincl.o: fixincl.x

fixincl: $(OBJ)
	if $(CC) -o $@ $(OBJ) $(LIB) ; then : ; else \
	rm -f $@ ; echo false > $@ ; fi

regex.o: regex.c
	$(CC) -g -DSTDC_HEADERS=1 -c regex.c

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

fixincl.sh: inclhack.def inclhack.tpl fixincl inclhack.sh
	@if ( ./fixincl -v > /dev/null 2>&1 ) ; then \
		if ( autogen --help > /dev/null 2>&1 ) ; then \
		echo autogen -DPROGRAM=1 -b fixincl inclhack.def ; \
		autogen -DPROGRAM=1 -b fixincl inclhack.def ; touch $@ ; \
		else echo You need to install autogen ; touch $@ ; fi ; \
	else cp inclhack.sh $@ ; echo cp inclhack.sh $@ ; fi

clean:
	rm -f *.o fixincl

install: fixincl.sh
	@rm -f ../../gcc/fixincl.sh ; \
		echo cp fixincl.sh ../../gcc ; \
		cp fixincl.sh ../../gcc ; \
		chmod +x ../../gcc/fixincl.sh
	@echo check for fixincl copy...
	@if ( ./fixincl -v > /dev/null 2>&1 ) ; then \
		echo cp fixincl ../../gcc/fixincl ; \
		cp fixincl ../../gcc/fixincl ; fi
