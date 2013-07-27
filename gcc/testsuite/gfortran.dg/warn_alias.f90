! { dg-do compile }
! { dg-options "-Waliasing" }
!
! PR fortran/57991
!
! Added check for OUT/OUT. IN/OUT and OUT/IN where already check
! since GCC 4.0, but not being tested for.

      Program q
        integer :: x
        x = 5
        Call test1(x, x) ! { dg-warning "Same actual argument associated with INTENT.OUT. argument 'a' and INTENT.OUT. argument 'b'" }
        Call test2(x, x) ! { dg-warning "Same actual argument associated with INTENT.IN. argument 'a' and INTENT.OUT. argument 'b'" }
        Call test3(x, x) ! { dg-warning "Same actual argument associated with INTENT.OUT. argument 'a' and INTENT.IN. argument 'b'" }
      Contains
        Subroutine test1(a,b)
          Integer, intent(out) :: a
          Integer, intent(out) :: b
          b = 5
          a = 5
        End Subroutine
        Subroutine test2(a,b)
          Integer, intent(in) :: a
          Integer, intent(out) :: b
          b = 5 + a
        End Subroutine
        Subroutine test3(a,b)
          Integer, intent(out) :: a
          Integer, intent(in) :: b
          a = 5 + b
        End Subroutine
      End Program

