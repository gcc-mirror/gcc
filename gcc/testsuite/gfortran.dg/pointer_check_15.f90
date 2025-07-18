! { dg-do run }
! { dg-additional-options "-O -fcheck=pointer -fdump-tree-original" }
!
! PR fortran/121145
! Erroneous runtime error: Proc-pointer actual argument 'ptr' is not associated
!
! Contributed by Federico Perini.

module m
  implicit none

  abstract interface
     subroutine fun(x)
       real, intent(in) :: x
     end subroutine fun
  end interface   

contains

  subroutine with_fun(sub)
    procedure(fun), optional :: sub
    if (present(sub)) stop 1
  end subroutine   

  subroutine with_non_optional(sub)
    procedure(fun) :: sub
  end subroutine   

end module m

program p
  use m
  implicit none

  procedure(fun), pointer :: ptr1 => null()
  procedure(fun), pointer :: ptr2 => null()
  
  call with_fun()
  call with_fun(sub=ptr1)               ! no runtime check here

  if (associated (ptr2)) then
     call with_non_optional(sub=ptr2)   ! runtime check here
  end if
end  

! { dg-final { scan-tree-dump-times "Proc-pointer actual argument .'ptr2.'" 1 "original" } }
