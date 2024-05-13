! { dg-do run }
! Test the fix for comment 1 in PR113363, which failed as in comments below.
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
program p
  implicit none
  class(*), allocatable :: x(:), y
  character(*), parameter :: arr(2) = ["hello ","bye   "], &
                             sca = "Have a nice day"
  character(10) :: const

! Bug was detected in polymorphic array function results
  allocate(x, source = foo ())
  call check1 (x, arr)              ! Wrong output "6 hello e"
  deallocate (x)
  x = foo ()
  call check1 (x, arr)              ! Wrong output "0  "
  associate (var => foo ())         ! OK after r14-9489-g3fd46d859cda10
    call check1 (var, arr)          ! Now OK - outputs: "6 hello bye   "
  end associate

! Check scalar function results     ! All OK
  allocate (y, source = bar())
  call check2 (y, sca)
  deallocate (y)
  y = bar ()
  call check2 (y, sca)
  deallocate (y)
  associate (var => bar ())
    call check2 (var, sca)
  end associate

! Finally variable expressions...
  allocate (y, source = x(1))       ! Gave zero length here
  call check2 (y, "hello")
  y = x(2)                          ! Segfaulted here
  call check2 (y, "bye   ")
  associate (var => x(2))           ! Gave zero length here
    call check2 (var, "bye   ")
  end associate

! ...and constant expressions       ! All OK
  deallocate(y)
  allocate (y, source = "abcde")
  call check2 (y, "abcde")
  const = "hijklmnopq"
  y = const
  call check2 (y, "hijklmnopq")
  associate (var => "mnopq")
    call check2 (var, "mnopq")
  end associate
  deallocate (x, y)

contains

  function foo() result(res)
    class(*), allocatable :: res(:)
    res = arr
  end function foo

  function bar() result(res)
    class(*), allocatable :: res
    res = sca
  end function bar

  subroutine check1 (x, carg)
    class(*), intent(in) :: x(:)
    character(*) :: carg(:)
    select type (x)
    type is (character(*))
      if (any (x .ne. carg)) stop 1
    class default
       stop 2
    end select
  end subroutine check1

  subroutine check2 (x, carg)
    class(*), intent(in) :: x
    character(*) :: carg
    select type (x)
    type is (character(*))
      if (x .ne. carg) stop 3
    class default
       stop 4
    end select
  end subroutine check2
end
