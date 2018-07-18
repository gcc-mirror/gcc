! { dg-do compile }
! { dg-options "-fcoarray=lib" }

program ptr_comp 
   type t
      integer, pointer :: z(:)
   end type
   type(t), save :: obj[*]
   integer, allocatable, target :: i(:)[:]

   obj%z => i(:)[4] ! { dg-error "shall not have a coindex" }
end program

