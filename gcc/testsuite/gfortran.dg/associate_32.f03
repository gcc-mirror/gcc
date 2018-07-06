! { dg-do run }
!
! Tests fix for PR77296 and other bugs found on the way.
!
! Contributed by Matt Thompson  <matthew.thompson@nasa.gov>
!
program test

   implicit none
   type :: str_type
     character(len=:), allocatable :: str
   end type

   character(len=:), allocatable :: s, sd(:)
   character(len=2), allocatable :: sf, sfd(:)
   character(len=6) :: str
   type(str_type) :: string

   s = 'ab'
   associate(ss => s)
     if (ss .ne. 'ab') STOP 1! This is the original bug.
     ss = 'c'
   end associate
   if (s .ne. 'c ') STOP 2! No reallocation within ASSOCIATE block!

   sf = 'c'
   associate(ss => sf)
     if (ss .ne. 'c ') STOP 3! This the bug in comment #2 of the PR.
     ss = 'cd'
   end associate

   sd = [s, sf]
   associate(ss => sd)
     if (any (ss .ne. ['c ','cd'])) STOP 4
   end associate

   sfd = [sd,'ef']
   associate(ss => sfd)
     if (any (ss .ne. ['c ','cd','ef'])) STOP 5
     ss = ['gh']
   end associate
     if (any (sfd .ne. ['gh','cd','ef'])) STOP 6! No reallocation!

   string%str = 'xyz'
   associate(ss => string%str)
     if (ss .ne. 'xyz') STOP 7
     ss = 'c'
   end associate
   if (string%str .ne. 'c  ') STOP 8! No reallocation!

   str = "foobar"
   call test_char (5 , str)
   IF (str /= "abcder") STOP 9

   associate(ss => foo())
     if (ss .ne. 'pqrst') STOP 10
   end associate

   associate(ss => bar())
     if (ss(2) .ne. 'uvwxy') STOP 11
   end associate

! The deallocation is not strictly necessary but it does allow
! other memory leakage to be tested for.
   deallocate (s, sd, sf, sfd, string%str)
contains

! This is a modified version of the subroutine in associate_1.f03.
! 'str' is now a dummy.
  SUBROUTINE test_char (n, str)
    INTEGER, INTENT(IN) :: n

    CHARACTER(LEN=n) :: str

    ASSOCIATE (my => str)
      IF (LEN (my) /= n) STOP 12
      IF (my /= "fooba") STOP 13
      my = "abcde"
    END ASSOCIATE
    IF (str /= "abcde") STOP 14
  END SUBROUTINE test_char

   function foo() result(res)
     character (len=:), pointer :: res
     allocate (res, source = 'pqrst')
   end function

   function bar() result(res)
     character (len=:), allocatable :: res(:)
     allocate (res, source = ['pqrst','uvwxy'])
   end function

end program test
