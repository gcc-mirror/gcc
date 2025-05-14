!{ dg-do run }

! Check that PR119272 is fixed
! Contributed by Xing Jing Wei  <xingjingwei666@gmail.com>

module pr119272_module
   type, public :: test_type
      contains
      procedure :: scal_function
      procedure :: arr_function
   end type test_type
   contains
   function scal_function(this) result(smth)
      class(test_type) :: this
      integer :: smth
      smth = 2
   end function
   function arr_function(this) result(smth)
      class(test_type) :: this
      integer :: smth(9)
      smth = (/(i, i=1, 9)/)
   end function
end module

program pr119272
      use pr119272_module
      implicit none
      
      type(test_type) :: a
              
      call test_subroutine(a)
      contains
      subroutine test_subroutine(a)
            class(test_type) :: a
            integer :: i
            integer,parameter :: temp_int(3) = [ 1, 2, 3]
            integer,parameter :: identity(9) = (/(i* 5, i= 9, 1, -1)/)
            associate(temp => temp_int(a%scal_function()))
                if (temp /= 2) stop 1
            end associate

            associate(temparr => identity(a%arr_function()))
                if (any(temparr /= (/(i* 5, i= 9, 1, -1)/))) stop 2
            end associate
      end subroutine
end program

