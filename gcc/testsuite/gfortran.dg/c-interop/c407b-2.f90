! PR 101337
! { dg-do compile}
!
! TS 29113
! C407b  An assumed-type variable name shall not appear in a designator
! or expression except as an actual argument corresponding to a dummy
! argument that is assumed-type, or as the first argument to any of
! the intrinsic and intrinsic module functions IS_CONTIGUOUS, LBOUND, 
! PRESENT, RANK, SHAPE, SIZE, UBOUND, and C_LOC.
!
! This file contains tests that are expected to give diagnostics.

! Check that passing an assumed-type variable as an actual argument 
! corresponding to a non-assumed-type dummy gives a diagnostic.

module m
  interface
    subroutine f (a, b)
      implicit none
      integer :: a
      integer :: b
    end subroutine
    subroutine g (a, b)
      implicit none
      type(*) :: a
      integer :: b
    end subroutine
    subroutine h (a, b)
      implicit none
      type(*) :: a(*)
      integer :: b
    end subroutine
  end interface
end module

subroutine s0 (x)
  use m
  implicit none
  type(*) :: x

  call g (x, 1)
  call f (x, 1)  ! { dg-error "Type mismatch" }
  call h (x, 1)  ! { dg-error "Rank mismatch" }
end subroutine

! Check that you can't use an assumed-type array variable in an array
! element or section designator.

subroutine s1 (x, y)
  use m
  implicit none
  integer :: x(*)
  type(*) :: y(*)

  call f (x(1), 1)
  call g (y(1), 1)  ! { dg-error "Assumed.type" }
  call h (y, 1)  ! ok
  call h (y(1:3:1), 1)  ! { dg-error "Assumed.type" }
end subroutine

! Check that you can't use an assumed-type array variable in other
! expressions.  This is clearly not exhaustive since few operations
! are even plausible from a type perspective.

subroutine s2 (x, y)
  implicit none
  type(*) :: x, y
  integer :: i

  ! select type
  select type (x) ! { dg-error "Assumed.type|Selector shall be polymorphic" }
    type is (integer)
      i = 0
    type is (real)
      i = 1 
    class default
      i = -1
  end select

  ! relational operations
  if (x & ! { dg-error "Assumed.type" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
      .eq. y) then  ! { dg-error "Assumed.type" } 
    return
  end if
  if (.not. (x & ! { dg-error "Assumed.type" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
             .ne. y)) then  ! { dg-error "Assumed.type" } 
    return
  end if
  if (.not. x) then  ! { dg-error "Assumed.type" } 
    return
  end if

  ! assignment
  x &  ! { dg-error "Assumed.type" } 
    = y  ! { dg-error "Assumed.type" } 
  i = x  ! { dg-error "Assumed.type" } 
  y = i  ! { dg-error "Assumed.type" } 

  ! arithmetic
  i = x + 1  ! { dg-error "Assumed.type" } 
  i = -y  ! { dg-error "Assumed.type" } 
  i = (x & ! { dg-error "Assumed.type" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
       + y)  ! { dg-error "Assumed.type" } 

  ! computed go to
  goto (10, 20, 30), x  ! { dg-error "Assumed.type|must be a scalar integer" }
10 continue
20 continue
30 continue

  ! do loops
  do i = 1, x   ! { dg-error "Assumed.type" }
    continue
  end do
  do x = 1, i   ! { dg-error "Assumed.type" }
    continue
  end do

end subroutine  

! Check that calls to disallowed intrinsic functions produce a diagnostic.
! Again, this isn't exhaustive, there are just too many intrinsics and
! hardly any of them are plausible.

subroutine s3 (x, y)
  implicit none
  type(*) :: x, y
  integer :: i

  i = bit_size (x)  ! { dg-error "Assumed.type" }
  i = exponent (x)  ! { dg-error "Assumed.type" }

  if (extends_type_of (x, &  ! { dg-error "Assumed.type" }
                       y)) then  ! { dg-error "Assumed.type" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    return
  end if

  if (same_type_as (x, &  ! { dg-error "Assumed.type" }
                    y)) then  ! { dg-error "Assumed.type" "pr101337, failure to diagnose both operands" { xfail *-*-*} }
    return
  end if

  i = storage_size (x)  ! { dg-error "Assumed.type" }

  i = iand (x, &  ! { dg-error "Assumed.type" }
            y)    ! { dg-error "Assumed.type" "pr101337, failure to diagnose both operands" { xfail *-*-*} }

  i = kind (x)  ! { dg-error "Assumed.type" }

end subroutine  
