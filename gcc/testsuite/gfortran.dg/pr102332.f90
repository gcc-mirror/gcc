! { dg-do compile }
! PR fortran/102332 - ICE in select_type_set_tmp
! Contributed by G.Steinmetz

program p
  type t
     real :: a, b
  end type
  class(t), allocatable :: x ! Valid
  select type (y => x)
  type is (t)
     y%a = 0
  end select
end

subroutine s0 (x)
  type t
     real :: a, b
  end type
  class(t) :: x ! Valid
  select type (y => x)
  type is (t)
     y%a = 0
  end select
end

subroutine s1
  type t
     real :: a, b
  end type
  class(t) :: x         ! { dg-error "must be dummy, allocatable or pointer" }
  select type (y => x)
  type is (t)
     y%a = 0
  end select
end

subroutine s3
  type t
     real :: a, b
  end type
  class(t) :: x         ! { dg-error "must be dummy, allocatable or pointer" }
  select type (y => x)
  class is (t)
     y%a = 0
  end select
end

subroutine s2
  type t
     real :: a, b
  end type
  class(t) :: x         ! { dg-error "must be dummy, allocatable or pointer" }
  select type (y => x)
  type default          ! { dg-error "Expected" }
     y%a = 0
  end select
end

subroutine s4
  type t
     real :: a, b
  end type
  class(t) :: x         ! { dg-error "must be dummy, allocatable or pointer" }
  select type (y => x)
  class default
     y%a = 0
  end select
end
