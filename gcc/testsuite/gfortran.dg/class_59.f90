! { dg-do compile }
!
! PR 71894: [OOP] ICE in gfc_add_component_ref, at fortran/class.c:227
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

subroutine s1
  type t
    integer :: n
  end type
  type(t) :: x
  class(t) :: y  ! { dg-error "must be dummy, allocatable or pointer" }
  x = y
end

subroutine s2
  type t
  end type
  class(t) :: x    ! { dg-error "must be dummy, allocatable or pointer" }
  class(t), allocatable :: y
  select type (y)
  type is (t)
    y = x
  end select
end
