! { dg-do compile }
! { dg-options "-std=f2018" }
! PR fortran/111880 - redundant warning of obsolescent COMMON with submodule

module third_party_module
  integer :: some_param
  common /not_my_code/ some_param   ! { dg-warning "COMMON block" }
end module third_party_module

module foo
  use third_party_module
  interface
    module subroutine bar()
    end subroutine bar
  end interface
end module foo

submodule (foo) foo_submod      ! We do not need a warning here!
contains
  module procedure bar
  end procedure bar
end submodule foo_submod
