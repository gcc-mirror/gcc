! { dg-do compile }
!
! PR fortran/92736
!
module m
  integer :: i, j
end module m

module m2
  integer :: i, k
end module m2

module mod
  use m, only: i
  interface
    module subroutine sb1()
    end subroutine sb1
  end interface
end

! Error: use 'i' both for m's 'i' and 'j'
submodule(mod) sub     ! { dg-error "Symbol 'i' at .1. conflicts with the rename symbol" }
  use m1, only: i => j ! { dg-error "Symbol 'i' at .1. conflicts with the rename symbol" }
end

module mod2
  use m, only: i
  interface
    module subroutine sb1()
    end subroutine sb1
  end interface
end

! Error: use 'i' both for m's 'i' and m2's 'k'
submodule(mod2) sub2   ! { dg-error "Symbol 'i' at .1. conflicts with the rename symbol" }
  use m2, only: i => k ! { dg-error "Symbol 'i' at .1. conflicts with the rename symbol" }
end


module mod3
  use m, only: i
  interface
    module subroutine sb1()
    end subroutine sb1
  end interface
end

! Error: use 'i' both for m's 'i' and m2's 'i'
submodule(mod3) sub3  ! { dg-error "Symbol 'i' at .1. conflicts with the symbol" }
  use m2, only: i     ! { dg-error "Symbol 'i' at .1. conflicts with the symbol" }
end


module mod4
  use m, only: mm => i, i
  interface
    module subroutine sb1()
    end subroutine sb1
  end interface
end

! OK
submodule(mod4) sub4
  use m, only: i
  use m, only: mm => i
end

module mod5
  use m, only: mm => i
  interface
    module subroutine sb1()
    end subroutine sb1
  end interface
end

! mm from both m2 and m
submodule(mod5) sub5    ! { dg-error "Symbol 'mm' at .1. conflicts with the rename symbol" }
  use m2, only: mm => i ! { dg-error "Symbol 'mm' at .1. conflicts with the rename symbol" }
end
