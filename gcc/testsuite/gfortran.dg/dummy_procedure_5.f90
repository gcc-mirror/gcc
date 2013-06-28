! { dg-do compile }
!
! PR 50517: gfortran must detect that actual argument type is different from dummy argument type (r178939)
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

program main

  type t
    integer g
  end type

  type u
    integer g
  end type

  type(u), external :: ufunc
  call sub(ufunc)            ! { dg-error "Type mismatch in function result" }

contains

  subroutine sub(tfunc)
    type(t), external :: tfunc
  end subroutine

end program 
