! { dg-do compile }
! { dg-options "-std=gnu" }
! PR100154 - ICE in gfc_conv_procedure_call, at fortran/trans-expr.c:6131

program p
  implicit none
  integer           :: n
  character, target :: c
  character(len=0)  :: c0
  character(len=:), allocatable :: cc
  n = fget(cc)
  n = fget('a')       ! { dg-error "must be a variable" }
  n = fget(c0)        ! { dg-error "must have length at least 1" }
  call fget('x')      ! { dg-error "must be a variable" }
  n = fgetc(5,'a')    ! { dg-error "must be a variable" }
  call fgetc(5,c0)    ! { dg-error "must have length at least 1" }
  call fgetc(5,c,1)   ! { dg-error "must be a variable" }
  call fputc(5,'x',1) ! { dg-error "must be a variable" }
  n = fget (ptr_returning_func())
  print *, c
contains
  function ptr_returning_func () result (res)
    character, pointer :: res
    res => c
  end
end
