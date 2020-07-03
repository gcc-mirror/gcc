! { dg-do compile }
! PR fortran/95880 - ICE in gfc_add_type, at fortran/symbol.c:2030 

module m
end
block data
   use m
   integer m    ! { dg-error "cannot have a type" }
end block data
