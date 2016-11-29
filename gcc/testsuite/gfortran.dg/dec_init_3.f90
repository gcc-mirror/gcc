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

subroutine sub
  structure /s1/
    integer i
  end structure

  structure /s2/
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
      map
        character(32) s
        record /s1/ r
      end map
    end union
  end structure
  record /s2/ x
  call dummy (x.m11, x.m12, x.m13, x.m14, x.m24, x.m23, x.m22, x.m21)
  print *, x.r.i
  if ( x.r.i .ne. 0 ) then
    call abort ()
  endif
end subroutine

call sub

end
