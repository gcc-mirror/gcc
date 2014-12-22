! { dg-do compile }
!
! PR 63363: No diagnostic for passing function as actual argument to KIND
!
! Contributed by Ian Harvey <ian_harvey@bigpond.com>

  type :: t
  end type
  type(t) :: d
  class(*), allocatable :: c

  print *, KIND(d)    ! { dg-error "must be of intrinsic type" }
  print *, KIND(c)    ! { dg-error "must be of intrinsic type" }

  print *, KIND(f)    ! { dg-error "must be a data entity" }
  print *, KIND(f())
  print *, KIND(s)    ! { dg-error "must be a data entity" }
contains
  FUNCTION f()
    INTEGER(SELECTED_INT_KIND(4)) :: f
  END FUNCTION
  subroutine s
  end subroutine
END
