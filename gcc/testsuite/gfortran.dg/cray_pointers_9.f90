! { dg-do compile }
! { dg-options "-fcray-pointer" }
!
! Test the fix for PR36703 in which the Cray pointer was not passed
! correctly so that the call to 'fun' at line 102 caused an ICE.
!
! Contributed by James van Buskirk on com.lang.fortran
! http://groups.google.com/group/comp.lang.fortran/msg/b600c081a3654936
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!
module funcs
   use ISO_C_BINDING           ! Added this USE statement
   implicit none
! Interface block for function program fptr will invoke
! to get the C_FUNPTR
   interface
      function get_proc(mess) bind(C,name='BlAh')
         use ISO_C_BINDING
         implicit none
         character(kind=C_CHAR) mess(*)
         type(C_FUNPTR) get_proc
      end function get_proc
   end interface
end module funcs

module other_fun
   use ISO_C_BINDING
   implicit none
   private
! Message to be returned by procedure pointed to
! by the C_FUNPTR
   character, allocatable, save :: my_message(:)
! Interface block for the procedure pointed to
! by the C_FUNPTR
   public abstract_fun
   abstract interface
      function abstract_fun(x)
         use ISO_C_BINDING
         import my_message
         implicit none
         integer(C_INT) x(:)
         character(size(my_message),C_CHAR) abstract_fun(size(x))
      end function abstract_fun
   end interface
   contains
! Procedure to store the message and get the C_FUNPTR
      function gp(message) bind(C,name='BlAh')
         character(kind=C_CHAR) message(*)
         type(C_FUNPTR) gp
         integer(C_INT64_T) i

         i = 1
         do while(message(i) /= C_NULL_CHAR)
            i = i+1
         end do
	 allocate (my_message(i+1))      ! Added this allocation
         my_message = message(int(1,kind(i)):i-1)
         gp = get_funloc(make_mess,aux)
      end function gp

! Intermediate procedure to pass the function and get
! back the C_FUNPTR
      function get_funloc(x,y)
         procedure(abstract_fun) x
         type(C_FUNPTR) y
         external y
         type(C_FUNPTR) get_funloc

         get_funloc = y(x)
      end function get_funloc

! Procedure to convert the function to C_FUNPTR
      function aux(x)
         interface
            subroutine x() bind(C)
            end subroutine x
         end interface
         type(C_FUNPTR) aux

         aux = C_FUNLOC(x)
      end function aux

! Procedure pointed to by the C_FUNPTR
      function make_mess(x)
         integer(C_INT) x(:)
         character(size(my_message),C_CHAR) make_mess(size(x))

         make_mess = transfer(my_message,make_mess(1))
      end function make_mess
end module other_fun

program fptr
   use funcs
   use other_fun
   implicit none
   procedure(abstract_fun) fun        ! Removed INTERFACE
   pointer(p,fun)
   type(C_FUNPTR) fp

   fp = get_proc('Hello, world'//achar(0))
   p = transfer(fp,p)
   write(*,'(a)') fun([1,2,3])
end program fptr
! { dg-final { cleanup-modules "funcs other_fun" } }
