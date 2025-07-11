! { dg-do compile }
! { dg-options "-std=f2008" }
! { dg-shouldfail "Invalid use of IMPORT" }
! Test invalid uses of import
! Wording of some error messages change for -std>=F2018 but all are caught.
! PR fortran/29601

subroutine test()
  type myType3
    import ! { dg-error "only permitted in an INTERFACE body" }
    sequence
    integer :: i
  end type myType3
end subroutine test

program foo
  import ! { dg-error "only permitted in an INTERFACE body" }
  type myType
    sequence
    integer :: i
  end type myType
  type myType3
    sequence
    integer :: i
  end type myType3
  interface
    import ! { dg-error "only permitted in an INTERFACE body" }
    subroutine bar()
      import foob ! { dg-error "Cannot IMPORT 'foob' from host scoping unit" }
    end subroutine bar
    subroutine test()
      import :: ! { dg-error "Expecting list of named entities" }
    end subroutine test
  end interface
end program foo
