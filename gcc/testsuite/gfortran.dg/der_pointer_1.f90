! { dg-do compile }
! PR13010
! Arrays of self-referential pointers
module test
   type list_t
      type(list_t), pointer :: next
   end type list_t

   type listptr_t
      type(list_t), pointer :: this
   end type listptr_t

   type x_t
      type(listptr_t), pointer :: arr(:)
   end type x_t

   type(x_t), pointer :: x
end module test
