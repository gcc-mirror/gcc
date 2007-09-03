! { dg-do run }
! PR33253 namelist: reading back a string
! Test case modified from that of the PR by
! Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
  implicit none
  character(len=8) :: a
  namelist /foo/ a
  open(10, status="scratch")
  a = "a'a"
  write(10,foo) 
  rewind 10
  a = ""
  read (10,foo) ! This gave a runtime error before the patch.
  if (a.ne."a'a") call abort
  close (10)
end program main
