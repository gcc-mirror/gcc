! { dg-do run }
! PR33672 Additional runtime checks needed for namelist reads
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>

module global
  type             ::  mt
    character(len=2) ::  ch(2) = (/"aa","bb"/)
  end type mt
  type             ::  bt
    integer        ::  i(2) = (/1,2/)
    type(mt)       ::  m(2)
  end type bt
end module global

program namelist_40
  use global
  type(bt)         ::  x(2)
  character(40)    ::  teststring 
  namelist /mynml/ x

  teststring = " x(2)%m%ch(:)(2:2) = 'z','z',"
  call writenml (teststring)
  teststring = " x(2)%m(2)%ch(:)(2) = 'z','z',"
  call writenml (teststring)
  teststring = " x(2)%m(2)%ch(:)(:3) = 'z','z',"
  call writenml (teststring)
  teststring = " x(2)%m(2)%ch(1:2)(k:) = 'z','z',"
  call writenml (teststring)
  
contains

subroutine writenml (astring)
  character(40), intent(in)  :: astring
  character(300)   :: errmessage
  integer          :: ierror

  open (10, status="scratch", delim='apostrophe')
  write (10, '(A)') "&MYNML"
  write (10, '(A)') astring
  write (10, '(A)') "/"
  rewind (10)
  read (10, nml = mynml, iostat=ierror, iomsg=errmessage)
  if (ierror == 0) call abort
  print '(a)', trim(errmessage)
  close (10)

end subroutine writenml

end program namelist_40
! { dg-output "Multiple sub-objects with non-zero rank in namelist object x(\n|\r\n|\r)" }
! { dg-output "Missing colon in substring qualifier for namelist variable x%m%ch(\n|\r\n|\r)" }
! { dg-output "Substring out of range for namelist variable x%m%ch(\n|\r\n|\r)" }
! { dg-output "Bad character in substring qualifier for namelist variable x%m%ch(\n|\r\n|\r)" }
! { dg-final { cleanup-modules "global" } }
