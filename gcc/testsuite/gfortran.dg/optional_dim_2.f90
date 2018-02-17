! { dg-do run }
! PR33317 CSHIFT/EOSHIFT: Rejects optional dummy for DIM=
! Test case submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program test
 implicit none
 call sub(bound=.false., dimmy=1_8)
 call sub()
contains
 subroutine sub(bound, dimmy)
   integer(kind=8), optional :: dimmy
   logical, optional :: bound
   logical :: lotto(4)
   character(20) :: testbuf
   lotto = .false.
   lotto = cshift((/.true.,.false.,.true.,.false./),1,dim=dimmy)
   write(testbuf,*) lotto
   if (trim(testbuf).ne." F T F T") STOP 1
   lotto = .false.
   lotto = eoshift((/.true.,.true.,.true.,.true./),1,boundary=bound,dim=dimmy)
   lotto = eoshift(lotto,1,dim=dimmy)
   write(testbuf,*) lotto
   if (trim(testbuf).ne." T T F F") STOP 2
 end subroutine
end program test