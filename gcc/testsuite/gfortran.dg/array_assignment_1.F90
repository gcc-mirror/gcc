! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! Test that different array assignments work even when interleaving,
! reversing etc.  Make sure the results from assignment with constants
! as array triples and runtime array triples (where we always create
! a temporary) match.
#define TST(b,c,d,e,f,g,r) a=init; a(b:c:d) = a(e:f:g); \
       write(unit=line ,fmt="(9I1)") a;\
       if (line /= r) call abort ; \
       call mytst(b,c,d,e,f,g,r);

program main
  implicit none
  integer :: i
  integer, parameter :: n=9
  integer,  dimension(n) :: a
  character(len=n) :: line
  integer, dimension(n), parameter :: init = (/(i,i=1,n)/)
  TST(2,n,2,1,n-1,2,'113355779')
  TST(3,9,3,2,6,2,'122454786');
  TST(1,8,2,3,9,2,'325476989');
  TST(1,6,1,4,9,1,'456789789');
  TST(9,5,-1,1,5,1,'123454321');
  TST(9,5,-2,1,5,2,'123456381');
  TST(5,9,2,5,1,-2,'123456381');
  TST(1,6,1,2,7,1,'234567789');
  TST(2,7,1,1,6,1,'112345689');
end program main

subroutine mytst(b,c,d,e,f,g,r)
  integer,intent(in) :: b,c,d,e,f,g
  character(len=9), intent(in) :: r
  character(len=9) :: line
  integer, dimension(9) :: a
  a = (/(i,i=1,9)/)
  a(b:c:d) = a(e:f:g)
  write (unit=line,fmt='(9I1)') a
  if (line /= r) call abort
end subroutine mytst
