! { dg-do compile }
program demo_setval
   call setval(value)
   write(*,*)'VALUE=',value
   contains
      subroutine setval(value)
         class(*),intent(in) :: value
         select type(value)
            type is (integer)
               value = 10     ! { dg-error "in variable definition context" }
            type is (real)
               value = 10.20  ! { dg-error "in variable definition context" }
         end select
      end subroutine setval
end program demo_setval
