! { dg-do run { target fd_truncate } }
! PR33253 namelist: reading back a string, also fixed writing with delimiters.
! Test case modified from that of the PR by
! Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
  implicit none
  character(len=3) :: a
  namelist /foo/ a

  open(10, status="scratch", delim="quote")
  a = 'a"a'
  write(10,foo) 
  rewind 10
  a = ""
  read (10,foo) ! This gave a runtime error before the patch.
  if (a.ne.'a"a') call abort
  close (10)

  open(10, status="scratch", delim="apostrophe")
  a = "a'a"
  write(10,foo) 
  rewind 10
  a = ""
  read (10,foo)
  if (a.ne."a'a") call abort
  close (10)

  open(10, status="scratch", delim="none")
  a = "a'a"
  write(10,foo) 
  rewind 10
  a = ""
  read (10,foo)
  if (a.ne."a'a") call abort
  close (10)
end program main
