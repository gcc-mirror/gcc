! { dg-do compile }
   subroutine sub1(x)
     integer, intent(in) :: x
     entry sub1_c(x) bind(c)
   end subroutine sub1

   subroutine sub2_c(x) bind(c)
     integer, intent(in) :: x
     entry sub2(x)
   end subroutine sub2_c

   subroutine sub3_c(x) bind(c)
     integer, intent(in) :: x
     entry sub3_c_c(x) bind(c)
   end subroutine sub3_c
