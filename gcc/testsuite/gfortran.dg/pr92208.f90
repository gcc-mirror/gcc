! { dg-do run }
!
! PR fortran/92208
!
! Contributed by Nils Reiche
!
program stringtest
  implicit none
  integer, parameter :: noVars = 2

!  print*, "varNames: ", createVarnames("var",noVars)
  call function1(noVars,createVarnames("var",noVars),"path")

contains

function createVarnames(string,noVars) result(stringArray)
  implicit none
  character(len=*),                        intent(in)  :: string
  integer,                                 intent(in)  :: noVars
  character(len=len_trim(string)+6), dimension(noVars) :: stringArray
  integer :: i
  do i=1,noVars
    write(stringArray(i),'(a,i0)') string, i
  enddo
end function createVarnames

subroutine function1(noVars,varNames,path)
  implicit none
  integer, intent(in)  :: noVars
  character(len=*), intent(in)  :: path
  character(len=*), dimension(noVars) :: varNames

  if (path /= 'path') stop 1
  if (any(varNames /= ['var1', 'var2'])) stop 2
  !print*, "function1-path    : ", trim(path)
  !print*, "function1-varNames: ", varNames
end subroutine function1

end program stringtest
