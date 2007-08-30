! { dg-do compile }
!
! Check whether MODULE PROCEDUREs are properly treated
! They need to be contained in a procedure, i.e. an
! interface in another procedure is invalid; they may, however,
! come from a use-associated procedure.
! (The PROCEDURE statement allows also for non-module procedures
!  if there is an explicit interface.)
!
! PR fortran/33228
!
module inclmod
  implicit none
  interface
    subroutine wrong1(a)
      integer :: a
    end subroutine wrong1
  end interface
  interface gen_incl
    module procedure ok1
  end interface gen_incl
  external wrong2
  external wrong3
  real wrong3
contains
  subroutine ok1(f)
    character :: f
  end subroutine ok1
end module inclmod

module a
  use inclmod
  implicit none
  interface gen
    subroutine ok1_a(a,b)
      integer :: a,b
    end subroutine ok1_a
    module procedure ok1, ok2_a
  end interface gen
contains
  subroutine ok2_a(a,b,c)
     integer :: a,b,c
  end subroutine ok2_a
end module a

module b
  use inclmod
  interface gen_wrong_0
    module procedure gen_incl  ! { dg-error "Cannot change attributes" }
  end interface gen_wrong_0
end module b

module c
  use inclmod
  interface gen_wrong_1
    module procedure wrong1  ! { dg-error "is not a module procedure" }
  end interface gen_wrong_1
end module c

module d
  use inclmod
  interface gen_wrong_2
    module procedure wrong2  ! { dg-error "Cannot change attributes" }
  end interface gen_wrong_2
end module d

module e
  use inclmod
  interface gen_wrong_3
    module procedure wrong3  ! { dg-error "Cannot change attributes" }
  end interface gen_wrong_3
end module e

module f
  implicit none
  interface
    subroutine wrong_a(a)
      integer :: a
    end subroutine wrong_a
  end interface
  interface gen_wrong_4
    module procedure wrong_a  ! { dg-error "is not a module procedure" }
  end interface gen_wrong_4
end module f

module g
  implicit none
  external wrong_b            ! { dg-error "has no explicit interface" }
  interface gen_wrong_5
    module procedure wrong_b  ! wrong, see above
  end interface gen_wrong_5
end module g

module h
  implicit none
  external wrong_c            ! { dg-error "has no explicit interface" }
  real wrong_c
  interface gen_wrong_6
    module procedure wrong_c  ! wrong, see above
  end interface gen_wrong_6
end module h

end

! { dg-final { cleanup-modules "a inclmod" } }
