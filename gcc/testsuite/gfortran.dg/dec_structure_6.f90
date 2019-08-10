! { dg-do run }
! { dg-options "-fdec-structure -fallow-invalid-boz" }
!
! Test old-style CLIST initializers in STRUCTURE.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

integer, parameter :: as = 3
structure /s8/
  character*20 c /"HELLO"/  ! ok
  integer*2 j /300_4/       ! ok, converted
  integer   k /65536_8/     ! ok, implicit
  integer*4 l /200000/      ! ok, types match
  integer   m(5) /5,4,3,2,1/! ok
  integer   n(5) /1,3*2,1/  ! ok, with repeat spec (/1,2,2,2,1/)
  integer   o(as) /as*9/    ! ok, parameter array spec
  integer   p(2,2) /1,2,3,4/! ok
  real      q(3) /1_2,3.5,2.4E-12_8/ ! ok, with some implicit conversions
  integer :: canary = z'3D3D3D3D'    ! { dg-warning "BOZ literal constant" }
end structure

record /s8/ r8

! Old-style (clist) initializers in structures
if ( r8.c /= "HELLO" ) call aborts ("r8.c")
if ( r8.j /= 300 ) call aborts ("r8.j")
if ( r8.k /= 65536 ) call aborts ("r8.k")
if ( r8.l /= 200000 ) call aborts ("r8.l")
if (     r8.m(1) /= 5 .or. r8.m(2) /= 4 .or. r8.m(3) /= 3 &
    .or. r8.m(4) /= 2 .or. r8.m(5) /= 1) &
  call aborts ("r8.m")
if (     r8.n(1) /= 1 .or. r8.n(2) /= 2 .or. r8.n(3) /= 2 .or. r8.n(4) /= 2 &
    .or. r8.n(5) /= 1) &
  call aborts ("r8.n")
if ( r8.o(1) /= 9 .or. r8.o(2) /= 9 .or. r8.o(3) /= 9 ) call aborts ("r8.o")
if (     r8.p(1,1) /= 1 .or. r8.p(2,1) /= 2 .or. r8.p(1,2) /= 3 &
    .or. r8.p(2,2) /= 4) &
  call aborts ("r8.p")
if ( r8.canary /= int(z'3D3D3D3D') ) call aborts ("r8.canary")

end
