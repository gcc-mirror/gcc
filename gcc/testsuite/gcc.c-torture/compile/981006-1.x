# For MIPS at least, pic is needed to trigger the problem.
if { [istarget rs6000-*-aix*]
     || [istarget powerpc*-*-aix*]
     || [istarget arm*-*-*]       
     || [istarget strongarm*-*-*]
     || [istarget fr30-*-*]
} {
    set options "-Wuninitialized -Werror"
} else {
    set options "-Wuninitialized -Werror -fpic"
}
return 0
