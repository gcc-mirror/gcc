! { dg-do compile }
! Tests the patch for PR27613, in which directly recursive, scalar
! functions were generating an "unclassifiable statement" error
! for the recursive statement(s).
!
! Based on PR testcase by Nicolas Bock  <nicolasbock@gmail.com>
!
program test
  if (original_stuff(1) .ne. 5) call abort ()
  if (scalar_stuff(-4) .ne. 10) call abort ()
  if (any (array_stuff((/-19,-30/)) .ne. (/25,25/))) call abort ()
contains
  recursive function original_stuff(n)
    integer :: original_stuff
    integer :: n
    original_stuff = 1
    if(n < 5) then
      original_stuff = original_stuff + original_stuff (n+1)
    endif
  end function original_stuff

  recursive function scalar_stuff(n) result (tmp)
    integer :: tmp
    integer :: n
    tmp = 1
    if(n < 5) then
      tmp = tmp + scalar_stuff (n+1)
    endif
  end function scalar_stuff

  recursive function array_stuff(n) result (tmp)
    integer :: tmp (2)
    integer :: n (2)
    tmp = 1
    if(maxval (n) < 5) then
      tmp = tmp + array_stuff (n+1)
    endif
  end function array_stuff

  recursive function bad_stuff(n)
    integer :: bad_stuff (2)
    integer :: n(2)
    bad_stuff = 1
    if(maxval (n) < 5) then
      bad_stuff = bad_stuff + bad_stuff (n+1) ! { dg-error "RESULT must be specified" }
    endif
  end function bad_stuff
end program test
