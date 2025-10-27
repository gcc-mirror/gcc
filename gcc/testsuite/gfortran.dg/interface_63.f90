! { dg-do compile }
! PR fortran/120784 - fix checking of renamed-on-use interface name
!
! Contributed by Matt Thompson  <matthew.thompson at nasa dot gov>

module A_mod
  implicit none

  interface Get
     procedure :: get_1
     procedure :: get_2
  end interface Get

contains

  subroutine get_1(i)
    integer :: i
    i = 5
  end subroutine get_1

  subroutine get_2(x)
    real :: x
    x = 4
  end subroutine get_2
end module A_mod

module B_mod
  use A_mod, only : MyGet => Get
  implicit none

  interface MyGet
     procedure :: other_get
  end interface MyGet

contains

  subroutine other_get(c)
    character(1) :: c
    c = 'a'
  end subroutine other_get

  subroutine check_get ()
    character :: c
    integer   :: i
    real      :: r
    call myget (c)
    call myget (i)
    call myget (r)
  end subroutine check_get

end module B_MOD

program p
  use b_mod, only: myget
  implicit none
  character :: c
  integer   :: i
  real      :: r
  call myget (c)
  call myget (i)
  call myget (r)
end

! Check that we do not regress on the following:

module mod1
  implicit none

  interface local
    module procedure local_data
  end interface local

contains

  logical function local_data (data) result (local)
    real, intent(in) :: data
    local = .true.
  end function local_data

end module mod1

module mod2
  use mod1, only: local
  implicit none

  interface local
     module procedure local_invt
  end interface local

contains

  logical function local_invt (invt) result (local)
    integer, intent(in) :: invt
    local = .true.
  end function local_invt

end module mod2
