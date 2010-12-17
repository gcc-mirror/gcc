! { dg-do compile }
!
! PR 44869: [OOP] Missing TARGET check - and wrong code or accepts-invalid?
!
! Contributed by Satish.BD <bdsatish@gmail.com>

  type :: test_case
  end type 

  type :: test_suite
    type(test_case) :: list
  end type

contains

  subroutine sub(self)
    class(test_suite), intent(inout) :: self
    type(test_case), pointer :: tst_case
    tst_case => self%list       ! { dg-error "is neither TARGET nor POINTER" }
  end subroutine

end
