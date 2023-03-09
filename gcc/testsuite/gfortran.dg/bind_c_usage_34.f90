! { dg-do compile }
! PR fortran/104332 - ICE with bind(c) in block data
! Contributed by G. Steinmetz

block data
  bind(c) :: a ! { dg-error "cannot be BIND\\(C\\)" }
end

block data aa
   real, bind(c) :: a ! { dg-error "cannot be BIND\\(C\\)" }
end

block data bb
   real    :: a ! { dg-error "cannot be BIND\\(C\\)" }
   bind(c) :: a
end

block data cc
   common /a/ x
   bind(c) :: /a/
end
