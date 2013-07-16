! { dg-do run }
!
! PR fortran/56737
!
! Contributed by Jonathan Hogg
!
module hsl_mc73_single
   implicit none
   integer, parameter, private :: wp = kind(0.0)
contains
   subroutine mc73_fiedler(n,lirn,irn,ip,list)
      integer,  intent (in) :: n
      integer,  intent (in) :: lirn
      integer,  intent (in) :: irn(*)
      integer,  intent (in) :: ip(*)
      integer, intent (out) :: list(*)

      integer :: icntl(10)

      call fiedler_graph(icntl)
   end subroutine mc73_fiedler

   subroutine mc73_order
      integer :: icntl(10)

      call fiedler_graph(icntl)
   end subroutine mc73_order

   subroutine fiedler_graph(icntl)
      integer,  intent (in) :: icntl(10)

      real (kind = wp)  :: tol
      real (kind = wp)  :: tol1
      real (kind = wp)  :: rtol

      call multilevel_eig(tol,tol1,rtol,icntl)
   end subroutine fiedler_graph

   subroutine multilevel_eig(tol,tol1,rtol,icntl)
      real (kind = wp), intent (in) :: tol,tol1,rtol
      integer,  intent(in) :: icntl(10)

      call level_print(6,'end of level ',1)
   end subroutine multilevel_eig

   subroutine level_print(mp,title1,level)
      character (len = *), intent(in) :: title1
      integer,  intent(in) :: mp,level
      character(len=80) fmt
      integer :: char_len1,char_len2

      char_len1=len_trim(title1)

      write (fmt,"('(',i4,'(1H ),6h===== ,a',i4,',i4,6h =====)')") &
           level*3, char_len1
!      print *, "fmt = ", fmt
!      print *, "title1= ", title1
!      print *, "level = ", level
      write (66,fmt) title1,level
   end subroutine level_print
end module hsl_mc73_single

program test
   use hsl_mc73_single
   implicit none
   character(len=200) :: str(2)
   integer, parameter :: wp = kind(0.0)

   integer :: n, lirn
   integer :: irn(1), ip(1), list(1)

   str = ""
   open (66, status='scratch')
   call mc73_order
   call mc73_fiedler(n,lirn,irn,ip,list)
   rewind (66)
   read (66, '(a)') str
   close (66)
   if (any (str /= "   ===== end of level   1 =====")) call abort()
end program test
