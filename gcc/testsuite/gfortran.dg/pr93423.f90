! { dg-do compile }
! PR fortran/93423 - ICE on invalid with argument list for module procedure

module t
  type :: b
   contains
     procedure :: p => bp
  end type b
  interface
     module function bp(s)
       class(b), intent(inout) :: s
       integer, pointer :: bp
     end function
  end interface
end module t

submodule (t) ts
contains
  module procedure bp(s) ! { dg-error "must be in a generic module interface" }
  end procedure bp       ! { dg-error "Expecting END SUBMODULE statement" }
end submodule ts
