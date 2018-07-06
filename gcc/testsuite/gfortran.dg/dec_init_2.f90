! { dg-do run }
! { dg-options "-fdec-structure -finit-derived -finit-integer=42 -finit-real=nan -finit-logical=true -finit-character=32" }
! { dg-add-options ieee }
!
! Test -finit-derived with DEC structure and union.
!

subroutine dummy(i1,r1,c1,l1,i2,r2,c2,l2)
  implicit none
  integer, intent(in) :: i1
  real, intent(in) :: r1
  character, intent(in) :: c1
  logical, intent(in) :: l1
  integer, intent(inout) :: i2
  real, intent(inout) :: r2
  character, intent(inout) :: c2
  logical, intent(inout) :: l2
  print *, i1, i2, l1, l2, ichar(c1), ichar(c2), r1, r2
  if ( i1 .ne. 42 .or. i2 .ne. 42 ) STOP 1
  if ( (.not. l1) .or. (.not. l2) ) STOP 2
  if ( c1 .ne. achar(32) .or. c2 .ne. achar(32) ) STOP 3
  if ( (.not. isnan(r1)) .or. (.not. isnan(r2)) ) STOP 4
end subroutine

! Nb. the current implementation decides the -finit-* flags are meaningless
! with components of a union, so we omit the union test here.

structure /s2/
  integer i2
  real r2
  character c2
  logical l2
end structure

structure /s1/
  logical l1
  real r1
  character c1
  integer i1
  record /s2/ y
end structure

record /s1/ x

call dummy (x.i1, x.r1, x.c1, x.l1, x.y.i2, x.y.r2, x.y.c2, x.y.l2)

end
