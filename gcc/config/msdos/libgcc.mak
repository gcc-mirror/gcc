# Build libgcc.a

libgcc.a : libgcc1.c libgcc2.c mklibgcc
	./mklibgcc -c
	./mklibgcc '$(CC) -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES)' libgcc1.c $(LIB1FUNCS)
	./mklibgcc '$(CC) -c $(ALL_CFLAGS) $(ALL_CPPFLAGS) $(INCLUDES)' libgcc2.c $(LIB2FUNCS)
	-command /c mklibnow.bat
	-command /c del libgcc.a
	ar rvs libgcc.a lgcctmp/*.o

mklibgcc : config/msdos/mklibgcc.c
	gcc $(CFLAGS) $^ -o $@


