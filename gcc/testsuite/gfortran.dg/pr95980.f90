! { dg-do compile }
! PR fortran/95980 - ICE in get_unique_type_string, at fortran/class.c:485

program p
  type t
  end type t
  class(t) :: x        ! { dg-error "must be dummy, allocatable or pointer" }
  select type (y => x)
  end select
end
