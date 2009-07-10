! { dg-do run }
! PR34540 cshift, eoshift, kind=1 and kind=2 arguments.
! Test case thanks to Thomas Koenig.
module tst_foo
  implicit none
contains
  subroutine tst_optional(a,n1,n2)
    integer(kind=1), intent(in), optional:: n1
    integer(kind=2), intent(in), optional:: n2
    integer(kind=1), dimension(2) :: s1
    character(64) :: testbuf
    real, dimension(:,:) :: a
    s1 = (/1, 1/)
    write(testbuf,'(4F10.2)') cshift(a, shift=s1)
    if (testbuf /= "      2.00      1.00      4.00      3.00") CALL abort
    write(testbuf,'(4F10.2)') cshift(a,shift=s1,dim=n2)
    if (testbuf /= "      2.00      1.00      4.00      3.00") CALL abort
    write(testbuf,'(4F10.2)') eoshift(a,shift=s1,dim=n1)
    if (testbuf /= "      2.00      0.00      4.00      0.00") CALL abort
    write(testbuf,'(4F10.2)') eoshift(a,shift=s1,dim=n2)
    if (testbuf /= "      2.00      0.00      4.00      0.00") CALL abort
  end subroutine tst_optional
 subroutine sub(bound, dimmy)
   integer(kind=8), optional :: dimmy
   logical, optional :: bound
   logical :: lotto(4)
   character(20) :: testbuf
   lotto = .false.
   lotto = cshift((/.true.,.false.,.true.,.false./),1,dim=dimmy)
   write(testbuf,*) lotto
   if (trim(testbuf).ne." F T F T") call abort
   lotto = .false.
   lotto = eoshift((/.true.,.true.,.true.,.true./),1,boundary=bound,dim=dimmy)
   lotto = eoshift(lotto,1,dim=dimmy)
   write(testbuf,*) lotto
   if (trim(testbuf).ne." T T F F") call abort
 end subroutine
end module tst_foo

program main
  use tst_foo
  implicit none
  real, dimension(2,2) :: r
  integer(kind=1) :: d1
  integer(kind=2) :: d2
  data r /1.0, 2.0, 3.0, 4.0/
  d1 = 1_1
  d2 = 1_2
  call tst_optional(r,d1, d2)
  call sub(bound=.false., dimmy=1_8)
  call sub()
end program main
! { dg-final { cleanup-modules "tst_foo" } }
