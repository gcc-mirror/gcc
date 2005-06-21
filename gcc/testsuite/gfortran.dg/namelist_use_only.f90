! { dg-do run }
! This tests the fix for PR22010, where namelists were not being written to
! and read back from modules.  It checks that namelists from modules that are
! selected by an ONLY declaration work correctly, even when the variables in
! the namelist are not host associated. Note that renaming a namelist by USE
! association is not allowed by the standard and this is trapped in module.c.
!
! Contributed by Paul Thomas  pault@gcc.gnu.org
!
module global
  character*4 :: aa, aaa
  integer :: ii, iii
  real    :: rr, rrr
  namelist /nml1/ aa, ii, rr
  namelist /nml2/ aaa, iii, rrr
contains
  logical function foo()
    foo = (aaa.ne."pqrs").or.(iii.ne.2).or.(rrr.ne.3.5)
  end function foo
end module global
program namelist_use_only
  use global, only : nml1, aa, ii, rr
  use global, only : nml2, rrrr=>rrr, foo
  open (10, status="scratch")
  write (10,*) "&NML1 aa=lmno ii=1 rr=2.5 /"
  write (10,*) "&NML2 aaa=pqrs iii=2 rrr=3.5 /"
  rewind (10)
  read (10,nml=nml1,iostat=i)
  if ((i.ne.0).or.(aa.ne."lmno").or.(ii.ne.1).or.(rr.ne.2.5)) call abort ()

  read (10,nml=nml2,iostat=i) 
  if ((i.ne.0).or.(rrrr.ne.3.5).or.foo()) call abort ()
  close (10)
end program namelist_use_only
