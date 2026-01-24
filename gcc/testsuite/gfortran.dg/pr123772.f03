! { dg-do run }
! Test case provided by Andrew Benson
module rmm
  private
  public :: rm

  type :: rm
     integer, pointer :: counter => null()
   contains
     final     :: rmDestructor
     procedure :: rmAssign
     generic   :: assignment(=) => rmAssign
     procedure :: getCounter => rmGetCounter
  end type rm

  interface rm
     module procedure rmConstructor
  end interface rm
contains
  function rmConstructor() result(self)
    implicit none
    type(rm) :: self
    allocate(self%counter)
    self%counter=1
    !write (*,'(a,i1)') '  rm construct - count = ',self%counter
    return
  end function rmConstructor

  subroutine rmDestructor(self)
    implicit none
    type(rm), intent(inout) :: self
    if (.not.associated(self%counter)) return
    self%counter=self%counter-1
    !write (*,'(a,i1)') '  rm destruct - count = ',self%counter
    nullify(self%counter )
    return
  end subroutine rmDestructor

  subroutine rmAssign(to,from)
    implicit none
    class(rm), intent(  out) :: to
    class(rm), intent(in   ) :: from
    if (associated(from%counter)) then
       to%counter => from%counter
       to%counter =  to  %counter+1
       !write (*,'(a,i1)') '  rm assign - count = ',to%counter
    else
       to%counter  => null()
    end if
    return
  end subroutine rmAssign

  integer function rmGetCounter(self)
    implicit none
    class(rm), intent(in) :: self
    rmGetCounter=self%counter
    return
  end function rmGetCounter
end module rmm

module hom
  use :: rmm, only : rm
  implicit none
  private
  public :: ho

  type ho
    private
    type(rm) :: fm
   contains
     final     ::                  hoDestructor
     procedure ::                  hoAssign
     generic   :: assignment(=) => hoAssign
     procedure :: getCounter => hoGetCounter
  end type ho

  interface ho
     module procedure hoConstructor
  end interface ho
contains
  subroutine hoDestructor(self)
    implicit none
    type(ho), intent(inout) :: self
    !write (*,'(a)') " ho destruct"    
    return
  end subroutine hoDestructor
  
  subroutine hoAssign(to,from)
    implicit none
    class(ho), intent(  out) :: to
    class(ho), intent(in   ) :: from

    !write (*,'(a)') " ho assign"    
    to%fm=from%fm
    return
  end subroutine hoAssign
  
  function hoConstructor() result(self)
    implicit none
    type(ho) :: self

    !write (*,'(a)') " ho construct"    
    self%fm=rm()
    return
  end function hoConstructor

  integer function hoGetCounter(self)
    implicit none
    class(ho), intent(in) :: self
    hoGetCounter=self%fm%getCounter()
    return
  end function hoGetCounter
    
end module hom

program bug
  use :: hom, only : ho 
  implicit none
  type(ho) :: fileObject
  !write (*,'(a)') "start"
  fileObject=ho()
  !write (*,'(a)') "end"  
  if (fileObject%getCounter() .ne. 1) stop 123
end program bug
