! { dg-do compile }
! Tests the fix for PR28959 in which interface derived types were
! not always being associated.
!
! Contributed by Salvatore Filippone  <sfilippone@uniroma2.it>
!
module derived_type_mod

  type foo_dtype
    integer, pointer :: v1(:)=>null()
  end type foo_dtype
  

end module derived_type_mod


Module tools

  interface foo_d_sub
     subroutine cdalv(m, v, i, desc_a, info, flag)
       use derived_type_mod
       Integer, intent(in)               :: m,i, v(:)
       integer, intent(in), optional     :: flag
       integer, intent(out)              :: info
       Type(foo_dtype), intent(out)  :: desc_a
     end subroutine cdalv
  end interface

end module tools



subroutine foo_bar(a,p,info)
  use derived_type_mod
  implicit none

  type(foo_dtype), intent(in)            :: a
  type(foo_dtype), intent(inout)         :: p
  integer, intent(out)                   :: info

  info=0

  call inner_sub(info)
    

  return


contains

  subroutine inner_sub(info)
    use tools
    implicit none 

    integer, intent(out)   :: info

    integer :: i, nt,iv(10)
    
    i  = 0
    nt = 1
    
    call foo_d_sub(nt,iv,i,p,info,flag=1)
    
    return


  end subroutine inner_sub



end subroutine foo_bar
