! { dg-do run }
!
! Tests fixes for various pr87477 dependencies
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de> except for pr102106:
! which was contributed by Brad Richardson  <everythingfunctional@protonmail.com>
!
program associate_60
  implicit none
  character(20) :: buffer

  call pr102106
  call pr100948
  call pr85686
  call pr88247
  call pr91941
  call pr92779
  call pr93339
  call pr93813

contains

  subroutine pr102106
    type :: sub_class_t
        integer :: i
    end type
    type :: with_polymorphic_component_t
        class(sub_class_t), allocatable :: sub_obj_
    end type
    associate(obj => with_polymorphic_component_t(sub_class_t(42)))
        if (obj%sub_obj_%i .ne. 42) stop 1
    end associate
  end

  subroutine pr100948
    type t
      character(:), allocatable :: c(:)
    end type
    type(t), allocatable :: x
!
! Valid test in comment 1
!
    x = t(['ab','cd'])
    associate (y => x%c(:))
      if (any (y .ne. x%c)) stop 2
      if (any (y .ne. ['ab','cd'])) stop 3
    end associate
    deallocate (x)
!
! Allocation with source was found to only copy over one of the array elements
!
    allocate (x, source = t(['ef','gh']))
    associate (y => x%c(:))
      if (any (y .ne. x%c)) stop 4
      if (any (y .ne. ['ef','gh'])) stop 5
    end associate
    deallocate (x)
  end

  subroutine pr85686
    call s85686([" g'day "," bye!! "])
    if (trim (buffer) .ne. " a g'day a bye!!") stop 6
  end

  subroutine s85686(x)
    character(*) :: x(:)
    associate (y => 'a'//x)
      write (buffer, *) y ! Used to segfault at the write statement.
    end associate
  end

  subroutine pr88247
      type t
         character(:), dimension(:), allocatable :: d
      end type t
      type(t), allocatable :: x
      character(5) :: buffer(3)
      allocate (x, source = t (['ab','cd'])) ! Didn't work
      write(buffer(1), *) x%d(2:1:-1)        ! Was found to be broken
      write(buffer(2), *) [x%d(2:1:-1)]      ! Was OK
      associate (y => [x%d(2:1:-1)])
        write(buffer(3), *) y                ! Bug in comment 7
      end associate
      if (any (buffer .ne. " cdab")) stop 7
  end

  subroutine pr91941
    character(:), allocatable :: x(:), z(:)
    x = [' abc', ' xyz']
    z = adjustl(x)
    associate (y => adjustl(x))              ! Wrong character length was passed
      if (any(y .ne. ['abc ', 'xyz '])) stop 8
    end associate
  end

  subroutine pr92779
    character(3) :: a = 'abc'
    associate (y => spread(trim(a),1,2) // 'd')
      if (any (y .ne. ['abcd','abcd'])) stop 9
    end associate
  end

  subroutine pr93339
    type t
      character(:), allocatable :: a(:)
    end type
    type(t) :: x
    x = t(["abc "])                    ! Didn't assign anything
!   allocate (x%a(1), source = 'abc') ! Worked OK
    associate (y => x%a)
       if (any (y .ne. 'abc ')) stop 10
          associate (z => x%a)
            if (any (y .ne. z)) stop 11
          end associate
    end associate
  end

  subroutine pr93813
    type t
    end type
    type, extends(t) :: t2
    end type
    class(t), allocatable :: x
    integer :: i = 0
    allocate (t :: x)
    associate (y => (x))  ! The parentheses triggered an ICE in select type
      select type (y)
      type is (t2)
          stop 12
      type is (t)
          i = 42
      class default
          stop 13
      end select
    end associate
    if (i .ne. 42) stop 14
  end
end
