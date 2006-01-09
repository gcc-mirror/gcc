! the problem here was that we had forgot to call
! fold_convert in gfc_trans_pointer_assign_need_temp
! so that we got a pointer to char instead of a 
! pointer to an array
! we really don't need a temp here.
! { dg-do compile }

      program test_forall
         type element
            character(32), pointer :: name
         end type element
         type(element)     :: charts(50)
         character(32), target :: names(50)
         forall(i=1:50)
            charts(i)%name => names(i)
         end forall
      end

