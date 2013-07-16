! { dg-do compile }
!
! PR 56968: [4.7/4.8/4.9 Regression] [F03] Issue with a procedure defined with a generic name returning procedure pointer
!
! Contributed by Samuel Debionne <samuel.debionne@ujf-grenoble.fr>

module test

  interface generic_name_get_proc_ptr
    module procedure specific_name_get_proc_ptr
  end interface

  abstract interface
    double precision function foo(arg1)
      real, intent(in) :: arg1
    end function
  end interface

contains

  function specific_name_get_proc_ptr() result(res)
    procedure(foo), pointer :: res
  end function

end module test

program crash_test
    use :: test

    procedure(foo), pointer :: ptr

    ptr => specific_name_get_proc_ptr()
    ptr => generic_name_get_proc_ptr()

end program

! { dg-final { cleanup-modules "test" } }
