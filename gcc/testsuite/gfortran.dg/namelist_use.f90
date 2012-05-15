! { dg-do run }
! This tests the fix for PR22010, where namelists were not being written to
! and read back from modules.  It has two namelists: one that is USE
! associated and another that is concatenated by USE and host association.
!
! Contributed by Paul Thomas  pault@gcc.gnu.org
!
module global
  character(4) :: aa
  integer :: ii
  real    :: rr
  namelist /nml1/ aa, ii, rr
  namelist /nml2/ aa
end module global
program namelist_use
  use global
  real    :: rrr
! Concatenate use and host associated variables - an extension.
  namelist /nml2/ ii, rrr    ! { dg-warning "already is USE associated" }
  open (10, status="scratch")
  write (10,*) "&NML1 aa='lmno' ii=1 rr=2.5 /"
  write (10,*) "&NML2 aa='pqrs' ii=2 rrr=3.5 /"
  rewind (10)
  read (10,nml=nml1,iostat=i)
  if ((i.ne.0).or.(aa.ne."lmno").or.(ii.ne.1).or.(rr.ne.2.5)) call abort ()

  read (10,nml=nml2,iostat=i) 
  if ((i.ne.0).or.(aa.ne."pqrs").or.(ii.ne.2).or.(rrr.ne.3.5)) call abort ()

  close (10)
end program namelist_use
