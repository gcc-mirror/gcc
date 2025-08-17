# Copyright (C) 2025 Free Software Foundation, Inc.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

OUT = ../rust-feature-defs.h

all: $(OUT)

mk-build-dir:
	mkdir -p build

build/parse.c: parse.y mk-build-dir
	$(YACC) $(YFLAGS) -o $@ --defines=build/parse.h $<

build/parse.h: build/parse.c;

build/scan.c: scan.l
	$(LEX) $(LFLAGS) -o $@ -Ca --header-file=build/scan.h $<

build/scan.h: build/scan.c;

build/%.o: build/%.c build/parse.h build/scan.h
	$(CC) $(CFLAGS) -c -Ibuild -o $@ $<

build/feature-extract: build/parse.o build/scan.o
	$(CC) $(LDFLAGS) $(LDLIBS) -o $@ $^

build/download.rs: fetch
	./$< $@

$(OUT): build/feature-extract build/download.rs
	# add copyright header + newline
	echo | \
	    cat copyright-stub.h - | \
	    sed "s/YYYY/$$(date +%Y)/" > build/rust-feature-defs.h
	cat build/download.rs | ./$< >> build/rust-feature-defs.h
	clang-format -i build/rust-feature-defs.h \
	    --style=file:../../../../../../contrib/clang-format
	mv build/rust-feature-defs.h $(OUT)

clean:
	$(RM) -r build

clean-all: clean
	$(RM) $(OUT)
