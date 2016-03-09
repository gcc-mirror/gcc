! { dg-do run }
!
! Test the fix for PR69423.
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!
program tester
  character(LEN=:), allocatable :: S
  S= test(2)
  if (len(S) .ne. 4) call abort
  if (S .ne. "test") call abort
  if (allocated (S)) deallocate (S)

  S= test2(2)
  if (len(S) .ne. 4) call abort
  if (S .ne. "test") call abort
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
    if (len(test) .ne. 4) call abort
    if (test .ne. "test") call abort
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
    if (len(test) .ne. 4) call abort
    if (test .ne. "test") call abort
  end function test2
end program tester
