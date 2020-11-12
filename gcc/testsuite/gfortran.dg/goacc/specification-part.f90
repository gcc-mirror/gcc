! { dg-do compile }
!
! PR fortran/90111
!
! Check that OpenACC directives in everywhere in specification part,
! i.e. it may appear before/after the use, import, implicit, and declaration
!

module m
end module m

subroutine foo0(kk)
  use m
  implicit none
  integer :: jj, kk
  !$acc routine
end

subroutine foo1()
  use m
  implicit none
  !$acc routine
  integer :: jj
end

subroutine foo2()
  use m
  !$acc routine
  implicit none
end

subroutine foo3()
  !$acc routine
  use m
  implicit none
end

module m2
  interface
    subroutine foo0(kk)
      use m
      import
      implicit none
      integer :: kk
      !$acc routine
    end
    subroutine foo1()
      use m
      import
      implicit none
      !$acc routine
    end
    subroutine foo2()
      use m
      import
      !$acc routine
      implicit none
    end
    subroutine foo3()
      use m
      !$acc routine
      import
      implicit none
    end
    subroutine foo4()
      use m
      !$acc routine
      import
      implicit none
    end
  end interface
end module m2

subroutine bar0()
  use m
  implicit none
  integer :: ii
  !$acc declare copyin(ii)
end

subroutine bar1()
  use m
  implicit none
  !$acc declare copyin(ii)
  integer :: ii
end

subroutine bar2()
  use m
  !$acc declare copyin(ii)
  implicit none
  integer :: ii
end

subroutine bar3()
  !$acc declare copyin(ii)
  use m
  implicit none
  integer :: ii
end
