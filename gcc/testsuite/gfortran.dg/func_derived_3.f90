! { dg-do run }
! This tests the "virtual fix" for PR19561, where pointers to derived
! types were not generating correct code.  This testcase is based on
! the original PR example.  This example not only tests the
! original problem but throughly tests derived types in modules,
! module interfaces and compound derived types.
!
! Original by Martin Reinecke  martin@mpa-garching.mpg.de  
! Submitted by Paul Thomas  pault@gcc.gnu.org
! Slightly modified by Tobias Schlüter
module func_derived_3
  implicit none
  type objA
    private
    integer :: i
  end type objA

  interface new
    module procedure oaInit
  end interface

  interface print
    module procedure oaPrint
  end interface

  private
  public objA,new,print

contains

  subroutine oaInit(oa,i)
    integer :: i
    type(objA) :: oa
    oa%i=i
  end subroutine oaInit

  subroutine oaPrint (oa)
    type (objA) :: oa
    write (10, '("simple  = ",i5)') oa%i
    end subroutine oaPrint

end module func_derived_3

module func_derived_3a
  use func_derived_3
  implicit none

  type objB
    private
    integer :: i
    type(objA), pointer :: oa
  end type objB

  interface new
    module procedure obInit
  end interface

  interface print
    module procedure obPrint
  end interface

  private
  public objB, new, print, getOa, getOa2

contains

  subroutine obInit (ob,oa,i)
    integer :: i
    type(objA), target :: oa
    type(objB) :: ob

    ob%i=i
    ob%oa=>oa
  end subroutine obInit

  subroutine obPrint (ob)
    type (objB) :: ob
    write (10, '("derived = ",i5)') ob%i
    call print (ob%oa)
  end subroutine obPrint

  function getOa (ob) result (oa)
    type (objB),target :: ob
    type (objA), pointer :: oa

    oa=>ob%oa
  end function getOa

! without a result clause 
  function getOa2 (ob)
    type (objB),target :: ob
    type (objA), pointer :: getOa2

    getOa2=>ob%oa
  end function getOa2
    
end module func_derived_3a

  use func_derived_3
  use func_derived_3a
  implicit none
  type (objA),target :: oa
  type (objB),target :: ob
  character (len=80) :: line

  open (10, status='scratch')

  call new (oa,1)
  call new (ob, oa, 2)

  call print (ob)
  call print (getOa (ob))
  call print (getOa2 (ob))
  
  rewind (10)
  read (10, '(80a)') line
  if (trim (line).ne."derived =     2") call abort ()
  read (10,  '(80a)') line
  if (trim (line).ne."simple  =     1") call abort ()
  read (10,  '(80a)') line
  if (trim (line).ne."simple  =     1") call abort ()
  read (10,  '(80a)') line
  if (trim (line).ne."simple  =     1") call abort ()
  close (10)
end program
