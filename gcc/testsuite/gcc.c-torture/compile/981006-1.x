# For MIPS at least, pic is needed to trigger the problem.
if { [istarget rs6000-*-aix*]
     || [istarget powerpc*-*-aix*]
     || [istarget arm*-*-*]
     || [istarget xscale-*-*]
     || [istarget strongarm*-*-*]
     || [istarget fr30-*-*]
     || [istarget sh-*-hms]
     || [istarget sh-*-coff]
     || [istarget h8300*-*-*]
     || [istarget cris-*-elf*]
     || [istarget cris-*-aout*]
     || [istarget mmix-*-*]
} {
    set options "-Wuninitialized -Werror"
} else {
    set options "-Wuninitialized -Werror -fpic"
}
return 0
