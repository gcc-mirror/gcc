! { dg-do compile }
!
! PR fortran/32616
!
! Check for to few elements of the actual argument
! and reject mismatching string lengths for assumed-shape dummies
!
implicit none
external test
integer :: i(10)
integer :: j(2,2)
character(len=4) :: str(2)
character(len=4) :: str2(2,2)

call test()

call foo(i(8)) ! { dg-warning "too few elements for dummy argument 'a' .3/4." }
call foo(j(1,1))
call foo(j(2,1)) ! { dg-warning "too few elements for dummy argument 'a' .3/4." }
call foo(j(1,2)) ! { dg-warning "too few elements for dummy argument 'a' .2/4." }

str = 'FORT'
str2 = 'fort'
call bar(str(:)(1:2)) ! { dg-warning "too few elements for dummy argument 'c' .4/6." }
call bar(str(1:2)(1:1)) ! { dg-warning "too few elements for dummy argument 'c' .2/6." }
call bar(str(2)) ! { dg-warning "too few elements for dummy argument 'c' .4/6." }
call bar(str(1)(2:1)) ! OK
call bar(str2(2,1)(4:1)) ! OK
call bar(str2(1,2)(3:4)) ! OK
call bar(str2(1,2)(4:4)) ! { dg-warning "too few elements for dummy argument 'c' .5/6." }
contains
  subroutine foo(a)
    integer :: a(4)
  end subroutine foo
  subroutine bar(c)
    character(len=2) :: c(3)
!    print '(3a)', ':',c(1),':'
!    print '(3a)', ':',c(2),':'
!    print '(3a)', ':',c(3),':'
  end subroutine bar
end


subroutine test()
implicit none
character(len=5), pointer :: c
character(len=5) :: str(5)
call foo(c) ! { dg-error "Character length mismatch" }
call bar(str) ! { dg-error "Character length mismatch" }
contains
  subroutine foo(a)
    character(len=3), pointer :: a
  end subroutine
  subroutine bar(a)
    character(len=3) :: a(:)
  end subroutine bar
end subroutine test
