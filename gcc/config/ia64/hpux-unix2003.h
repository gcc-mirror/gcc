
/* For HP-UX 11.31 and greater, use unix2003.o instead of unix98.o to
   get correct C99 snprintf behaviour with buffer overflow.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{static:crt0%O%s} \
			  %{mlp64:/usr/lib/hpux64/unix2003%O%s} \
			  %{!mlp64:/usr/lib/hpux32/unix2003%O%s}}"
