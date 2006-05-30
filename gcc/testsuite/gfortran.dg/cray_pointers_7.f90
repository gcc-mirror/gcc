! { dg-do run }
! { dg-options "-fcray-pointer" }

! Test the implementation of Cray pointers to procedures.
program cray_pointers_7
  implicit none
  integer tmp
  integer, external :: fn
  external sub
  
  ! We can't mix function and subroutine pointers.
  pointer (subptr,subpte)
  pointer (fnptr,fnpte)
  
  ! Declare pointee types.
  external subpte
  integer, external :: fnpte
  
  tmp = 0
  
  ! Check pointers to subroutines.
  subptr = loc(sub)
  call subpte(tmp)
  if (tmp .ne. 17) call abort()

  ! Check pointers to functions.
  fnptr = loc(fn)
  tmp = fnpte(7)
  if (tmp .ne. 14) call abort()
  
end program cray_pointers_7

! Trivial subroutine to be called through a Cray pointer.
subroutine sub(i)
  integer i
  i = 17
end subroutine sub

! Trivial function to be called through a Cray pointer.
function fn(i)
  integer fn,i
  fn = 2*i
end function fn
