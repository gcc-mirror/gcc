! { dg-do compile }
! Test fix for regression caused by r14-9489 - invalid code.
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>

module q
  type :: s
    integer :: j
  end type
  type :: t
    integer :: i
    class(s), allocatable :: ca
  end type
contains
  subroutine foobar
    class(*), allocatable :: c
    c = t (1)
    select type (c)
      type is (t)
! Regression caused ICE here in translation or error was missed - invalid array reference
        if (c(1)%i .ne. 1) stop 5         ! { dg-error "Syntax error in IF-expression" }
        if (allocated (c%ca)) then
! Make sure that response is correct if problem is "nested".
           select type (ca => c%ca)
             type is (s)
! Regression caused ICE here in translation or error was missed - invalid array reference
               if (ca(1)%j .ne. 1) stop 6 ! { dg-error "Syntax error in IF-expression" }
           end select
           select type (ca(1) => c%ca)    ! { dg-error "parse error in SELECT TYPE" }
             type is (s)                  ! { dg-error "Unexpected TYPE IS statement" }
               if (ca(1)%j .ne. 1) stop 6 ! { dg-error "nonderived-type variable" }
           end select                     ! { dg-error " Expecting END IF statement" }
        endif
    end select

! This problem was found in the course of the fix: Chunk taken from associate_64.f90,
! the derived type and component names adapted and the invalid array reference added.
    associate (var4 => bar4())
      if (var4%i .ne. 84) stop 33
      if (var4%ca%j .ne. 168) stop 34
      select type (x => var4)
        type is (t)
          if (x(1)%i .ne. var4%i) stop 35 ! { dg-error "Invalid array reference" }
          if (x%ca%j .ne. var4%ca%j) stop 36
        class default
          stop 37
      end select
    end associate
  end
  function bar4() result(res)
    class(t), allocatable :: res
    res = t(84, s(168))
  end
end module q
