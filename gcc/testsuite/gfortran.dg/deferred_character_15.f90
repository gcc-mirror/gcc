! { dg-do run }
!
! Test the fix for PR69423.
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!
program tester
  character(LEN=:), allocatable :: S
  S= test(2)
  if (len(S) .ne. 4) STOP 1
  if (S .ne. "test") STOP 2
  if (allocated (S)) deallocate (S)

  S= test2(2)
  if (len(S) .ne. 4) STOP 3
  if (S .ne. "test") STOP 4
  if (allocated (S)) deallocate (S)
contains
  function test(alen)
    character(LEN=:), allocatable :: test
    integer alen, i
    do i = alen, 1, -1
      test = 'test'
      exit
    end do
!       This line would print nothing when compiled with -O1 and higher.
!       print *, len(test),test
    if (len(test) .ne. 4) STOP 5
    if (test .ne. "test") STOP 6
  end function test

  function test2(alen) result (test)
    character(LEN=:), allocatable :: test
    integer alen, i
    do i = alen, 1, -1
      test = 'test'
      exit
    end do
!       This worked before the fix.
!       print *, len(test),test
    if (len(test) .ne. 4) STOP 7
    if (test .ne. "test") STOP 8
  end function test2
end program tester
