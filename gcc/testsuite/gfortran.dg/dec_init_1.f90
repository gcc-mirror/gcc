! { dg-do run }
! { dg-options "-fdec-structure -finit-derived -finit-local-zero" }
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
  print *, i1, i2, l1, l2, c1, c2, r1, r2
  if ( i1 .ne. 0 .or. i2 .ne. 0 ) call abort()
  if ( l1 .or. l2 ) call abort()
  if ( c1 .ne. achar(0) .or. c2 .ne. achar(0) ) call abort()
  if ( r1 .ne. 0.0 .or. r2 .ne. 0.0 ) call abort()
end subroutine

structure /s3/
  union
    map
      integer m11
      real m12
      character m13
      logical m14
    end map
    map
      logical m21
      character m22
      real m23
      integer m24
    end map
  end union
end structure

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
record /s3/ y

call dummy (x.i1, x.r1, x.c1, x.l1, x.y.i2, x.y.r2, x.y.c2, x.y.l2)
call dummy (y.m11, y.m12, y.m13, y.m14, y.m24, y.m23, y.m22, y.m21)

end
