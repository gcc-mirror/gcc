! { dg-do run }
!
! Test the fix of the test case referenced in comment 17 of PR83118.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  implicit none
  type Wrapper
    class(*), allocatable :: elements(:)
  end type
  type Mytype
    real(4) :: r = 42.0
  end type

  call driver
contains
  subroutine driver
    class(*), allocatable :: obj
    type(Wrapper) w
    integer(4) :: expected4(2) = [42_4, 43_4]
    integer(8) :: expected8(3) = [42_8, 43_8, 44_8]

    w = new_wrapper (expected4)
    obj = w
    call test (obj, 0)
    obj =  new_wrapper (expected8) ! Used to generate a linker error
    call test (obj, 10)
    obj = new_wrapper ([mytype (99.0)])
    call test (obj, 100)
    obj = Mytype (42.0) ! Used to generate a linker error
    call test (obj, 1000)
  end subroutine
  function new_wrapper(array) result (res)
    class(*) :: array(:)
    type(Wrapper) :: res
    res%elements = array ! Used to runtime segfault
  end function
  subroutine test (arg, idx)
    class(*) :: arg
    integer :: idx
    select type (arg)
      type is (wrapper)
        select type (z => arg%elements)
          type is (integer(4))
            if (any (z .ne. [42_4, 43_4])) stop 1 + idx
          type is (integer(8))
            if (any (z .ne. [42_8, 43_8, 44_8])) stop 1 + idx
          type is (Mytype)
            if (abs (z(1)%r - 99.0) .ge. 1e-6) stop 1 + idx
        class default
          stop 2 + idx
        end select
      type is (Mytype)
        if (abs (arg%r - 42.0) .ge. 1e-6) stop 1 + idx
      class default
        stop 3 + idx
    end select
  end subroutine
end
