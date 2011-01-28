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
    if (x .ne. y) call abort
    deallocate (x)
    x = y
    if (x .ne. y) call abort
  end subroutine   
  subroutine test_derived
    type :: mytype
      real :: x
      character(4) :: c
    end type
    type (mytype), allocatable :: t
    t = mytype (99.0, "abcd")
    if (t%c .ne. "abcd") call abort
  end subroutine   
  subroutine test_char1
    character(len = 8), allocatable :: c1
    character(len = 8) :: c2 = "abcd1234"
    c1 = "abcd1234"
    if (c1 .ne. c2) call abort
    deallocate (c1)
    c1 = c2
    if (c1 .ne. c2) call abort
  end subroutine    
  subroutine test_char4
    character(len = 8, kind = 4), allocatable :: c1
    character(len = 8, kind = 4) :: c2 = 4_"abcd1234"
    c1 = 4_"abcd1234"
    if (c1 .ne. c2) call abort
    deallocate (c1)
    c1 = c2
    if (c1 .ne. c2) call abort
  end subroutine
  subroutine test_deferred_char1  
    character(:), allocatable :: c
    c = "Hello"
    if (c .ne. "Hello") call abort
    if (len(c) .ne. 5) call abort
    c = "Goodbye"
    if (c .ne. "Goodbye") call abort
    if (len(c) .ne. 7) call abort
! Check that the hidden LEN dummy is passed by reference
    call test_pass_c1 (c)
    if (c .ne. "Made in test!") print *, c
    if (len(c) .ne. 13) call abort
  end subroutine
  subroutine test_pass_c1 (carg)
    character(:), allocatable :: carg
    if (carg .ne. "Goodbye") call abort
    if (len(carg) .ne. 7) call abort
    carg = "Made in test!"
  end subroutine
  subroutine test_deferred_char4
    character(:, kind = 4), allocatable :: c
    c = 4_"Hello"
    if (c .ne. 4_"Hello") call abort
    if (len(c) .ne. 5) call abort
    c = 4_"Goodbye"
    if (c .ne. 4_"Goodbye") call abort
    if (len(c) .ne. 7) call abort
! Check that the hidden LEN dummy is passed by reference
    call test_pass_c4 (c)
    if (c .ne. 4_"Made in test!") print *, c
    if (len(c) .ne. 13) call abort
  end subroutine
  subroutine test_pass_c4 (carg)
    character(:, kind = 4), allocatable :: carg
    if (carg .ne. 4_"Goodbye") call abort
    if (len(carg) .ne. 7) call abort
    carg = 4_"Made in test!"
  end subroutine
end

