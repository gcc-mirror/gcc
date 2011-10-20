! { dg-do compile }
! { dg-options "-Wno-align-commons" }
!
! PR fortran/45044
!
! Named common blocks need to be all of the same size
! check that the compiler warns for those.

module m
  common /xx/ a
end module m

subroutine two()
integer :: a, b, c
real(8) :: y
common /xx/ a, b, c, y ! { dg-warning "Named COMMON block 'xx' at \\(1\\) shall be of the same size as elsewhere \\(24 vs 4 bytes" }
end


subroutine one()
integer :: a, b
common /xx/ a, b ! { dg-warning "Named COMMON block 'xx' at \\(1\\) shall be of the same size as elsewhere \\(8 vs 24 bytes" }
end

call two()
end

! { dg-final { cleanup-modules "m" } }
