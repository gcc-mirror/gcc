# Generated automatically from Makefile.in by configure.
CFLAGS = -g -O2  -DHAVE_CONFIG_H
CC = gcc
INSTALL = /usr/bin/ginstall -c
prefix = /usr/local
exec_prefix = ${prefix}
BINARY = ${exec_prefix}/bin/fastjar

all: fastjar grepjar

fastjar: jartool.o dostime.o compress.o pushback.o
	$(CC) -o $@ jartool.o dostime.o compress.o pushback.o -lz 

grepjar: jargrep.o dostime.o compress.o pushback.o
	$(CC) -o $@ jargrep.o dostime.o compress.o pushback.o -lz 

install: fastjar
	${INSTALL} -s -m 755 fastjar $(BINARY)

uninstall:
	/bin/rm -f $(BINARY)

jartool.o: jartool.c dostime.c jartool.h zipfile.h dostime.h compress.h

jartool.c: jartool.h zipfile.h

dostime.o: dostime.c dostime.h

dostime.c: dostime.h

compress.o: compress.c compress.h

compress.c: compress.h

pushback.o: pushback.c pushback.h

pushback.c: pushback.h

jargrep.c: jargrep.h

jargrep.o: jargrep.c jargrep.h

clean:
	rm -rf *.o *~ core fastjar
