! { dg-do run }
! Test (re)allocation on assignment of scalars
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  call test_real
  call test_derived
  call test_char1
  call test_char4
  call test_deferred_char1
  call test_deferred_char4
contains
  subroutine test_real
    real, allocatable :: x
    real :: y = 42
    x = 42.0
    if (x .ne. y) STOP 1
    deallocate (x)
    x = y
    if (x .ne. y) STOP 2
  end subroutine   
  subroutine test_derived
    type :: mytype
      real :: x
      character(4) :: c
    end type
    type (mytype), allocatable :: t
    t = mytype (99.0, "abcd")
    if (t%c .ne. "abcd") STOP 3
  end subroutine   
  subroutine test_char1
    character(len = 8), allocatable :: c1
    character(len = 8) :: c2 = "abcd1234"
    c1 = "abcd1234"
    if (c1 .ne. c2) STOP 4
    deallocate (c1)
    c1 = c2
    if (c1 .ne. c2) STOP 5
  end subroutine    
  subroutine test_char4
    character(len = 8, kind = 4), allocatable :: c1
    character(len = 8, kind = 4) :: c2 = 4_"abcd1234"
    c1 = 4_"abcd1234"
    if (c1 .ne. c2) STOP 6
    deallocate (c1)
    c1 = c2
    if (c1 .ne. c2) STOP 7
  end subroutine
  subroutine test_deferred_char1  
    character(:), allocatable :: c
    c = "Hello"
    if (c .ne. "Hello") STOP 8
    if (len(c) .ne. 5) STOP 9
    c = "Goodbye"
    if (c .ne. "Goodbye") STOP 10
    if (len(c) .ne. 7) STOP 11
! Check that the hidden LEN dummy is passed by reference
    call test_pass_c1 (c)
    if (c .ne. "Made in test!") print *, c
    if (len(c) .ne. 13) STOP 12
  end subroutine
  subroutine test_pass_c1 (carg)
    character(:), allocatable :: carg
    if (carg .ne. "Goodbye") STOP 13
    if (len(carg) .ne. 7) STOP 14
    carg = "Made in test!"
  end subroutine
  subroutine test_deferred_char4
    character(:, kind = 4), allocatable :: c
    c = 4_"Hello"
    if (c .ne. 4_"Hello") STOP 15
    if (len(c) .ne. 5) STOP 16
    c = 4_"Goodbye"
    if (c .ne. 4_"Goodbye") STOP 17
    if (len(c) .ne. 7) STOP 18
! Check that the hidden LEN dummy is passed by reference
    call test_pass_c4 (c)
    if (c .ne. 4_"Made in test!") print *, c
    if (len(c) .ne. 13) STOP 19
  end subroutine
  subroutine test_pass_c4 (carg)
    character(:, kind = 4), allocatable :: carg
    if (carg .ne. 4_"Goodbye") STOP 20
    if (len(carg) .ne. 7) STOP 21
    carg = 4_"Made in test!"
  end subroutine
end

